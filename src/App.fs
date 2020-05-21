module App

// https://github.com/HackerNews/API

open Elmish
open Elmish.React
open Feliz

open Fable.SimpleHttp
open Thoth.Json



type ValueStatus<'t> =
    | NotStarted
    | InProgress
    | Resolved of 't


type AsyncStatus<'t> =
    | Started 
    | Finished of 't


type HackerNewsItem = {
    Id: int
    Title: string
    Url: string option
    Score: int
}


[<RequireQualifiedAccess>]
type Story =
    | New 
    | Top 
    | Best 
    | Job 


type State = {
    CurrentStory: Story
    StoryItems: ValueStatus<Result<HackerNewsItem list, string>>
}


type Msg =
    | ChangeStory of Story
    | LoadStoryItems of AsyncStatus<Result<HackerNewsItem list, string>>



let init() =
    let initState = { CurrentStory = Story.New; StoryItems = NotStarted }
    let initCommand = Cmd.ofMsg (LoadStoryItems Started)
    initState, initCommand


// ============================================== Helpers - Start
module Seq = 
    let values (chooser: 'a -> 'b option) (l: seq<'a>) : seq<'b> =
        let selectValues state item =
            match chooser item with
            | Some elem -> Seq.append state (Seq.singleton elem)
            | None   -> state

        Seq.fold selectValues Seq.empty l


module Cmd =
    let fromAsync (operation: Async<Msg>) : Cmd<Msg> =
        let command (dispatch: Msg -> unit) =
            let op = async {
                let! msg = operation
                return dispatch msg
            }

            Async.StartImmediate op

        Cmd.ofSub command


module Async =
    let map (f: 'a -> 'b) (operation: Async<'a>) =
        async {
            let! result = operation 
            return f result
        }


// https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty
let parseHackerNewsIds json =
    Decode.fromString (Decode.list Decode.int) json

// https://hacker-news.firebaseio.com/v0/item/21558013.json?print=pretty
let hackerNewsItemDecoder : Decoder<HackerNewsItem> =
    Decode.object (fun field ->
    {
        Id = field.Required.At [ "id" ] Decode.int
        Title = field.Required.At [ "title" ] Decode.string
        Url = field.Optional.At [ "url" ] Decode.string
        Score = field.Required.At [ "score" ] Decode.int
    })

 

let parseHackerNewsItem json =
    Decode.fromString hackerNewsItemDecoder json


let fetchStoryItem (id: int) =
    let url = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id

    async {
        let! status, responseText = Http.get url

        match status with 
        | 200 -> 
            match parseHackerNewsItem responseText with
            | Ok item -> return Some item
            | Error _ -> return None

        | _   -> return None
    }

let storiesEndpoint story =
    let fromBaseUrl = sprintf "https://hacker-news.firebaseio.com/v0/%sstories.json"

    match story with 
    | Story.Best -> fromBaseUrl "best"
    | Story.Top  -> fromBaseUrl "top"
    | Story.New  -> fromBaseUrl "new"
    | Story.Job  -> fromBaseUrl "job"


let fetchStoryItems (story: Story) =
    let url = storiesEndpoint story

    async {
        let! status, responseText = Http.get url

        match status with 
        | 200 ->
            match parseHackerNewsIds responseText with
            | Ok (storyIds) ->
                let! storyItems =
                    storyIds
                    |> List.truncate 10
                    // |> List.map ( fun _ -> async { return Some { Id = 1; Title = "Test"; Url = Some "url"; Score = 12 } })
                    |> List.map fetchStoryItem
                    |> Async.Parallel
                    |> Async.map (Array.choose id >> List.ofArray)

                return LoadStoryItems (Finished (Ok storyItems))

            | Error errorMsg ->
                // Could not parse the array of story IDs
                return LoadStoryItems (Finished (Error errorMsg))

        | _ ->
            // Non-OK response finishes with an error
            return LoadStoryItems (Finished (Error "Network error: Couldn't load stories"))
    }




// ============================================== Helpers - End


let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with 
    | LoadStoryItems Started ->
        let newState = { state with StoryItems = InProgress }

        newState, Cmd.fromAsync (fetchStoryItems state.CurrentStory)

    | LoadStoryItems (Finished (Ok storyItems)) ->
        let newState = { state with StoryItems = Resolved (Ok storyItems)}

        newState, Cmd.none

    | LoadStoryItems (Finished (Error error)) ->
        let newState = { state with StoryItems = Resolved (Error error)}

        newState, Cmd.none

    | ChangeStory story ->
        let newState = { state with CurrentStory = story; StoryItems = InProgress }

        newState, Cmd.fromAsync (fetchStoryItems story)


// ========================================================
// Render View
// ========================================================

let storyCategories = [
    Story.New 
    Story.Top
    Story.Best 
    Story.Job
]


let storyTitle story =
    match story with
    | Story.New -> "New"
    | Story.Top -> "Top"
    | Story.Best -> "Best"
    | Story.Job -> "Job"


let renderTabs selectedStory dispatch =
    // dispatch Msg.ChangeStories from the one of the tabs 
    // if it is not already selected.
    let switchStory story =
        if story <> selectedStory
        then dispatch (ChangeStory story)
        // else...?
        // Do nothing if the current story is active (visible on screen)
        // This is to make sure the clicking on the active tab won't reload the view.

    Html.div [
        prop.className [ "tabs"; "is-toggle"; "is-fullwidth" ]
        prop.children [
            Html.ul [
                for story in storyCategories ->
                    Html.li [
                        prop.className [ if story = selectedStory then "is-active" ]
                        prop.onClick (fun _ -> switchStory story)
                        prop.children [
                            Html.a [ Html.span (storyTitle story) ]
                        ]

                    ]
            ]
        ]
    ]


let renderError (errorMsg: string) = 
    Html.h1 [
        prop.style [ style.color.red ]
        prop.text errorMsg
    ]


let div (classes: string list) (children: ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]


let renderItem item =
    Html.div [
        prop.key item.Id
        prop.className "box"
        prop.style [ style.marginTop 15; style.marginBottom 15 ] 
        prop.children [
            div [ "columns"; "is-mobile" ] [
                div [ "column"; "is-narrow" ] [
                    Html.div [
                        prop.className [ "icon" ]
                        prop.style [ style.marginLeft 20 ]
                        prop.children [
                            Html.i [ prop.className "fa fa-poll fa-2x" ]
                            Html.span [
                                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                                prop.text (sprintf "Score: %d" item.Score)
                            ]
                        ]
                    ]
                ]

                div [ "column" ] [
                    match item.Url with 
                    | Some url ->
                        Html.a [
                            prop.style [ style.textDecoration.underline ]
                            prop.target.blank
                            prop.href url
                            prop.text item.Title
                        ]

                    | None -> Html.p item.Title
                ]
            ]

        ]
    ]


let spinner = 
    Html.div [
        prop.style [ style.textAlign.center; style.marginTop 20 ]
        prop.children [
            Html.i [
                prop.className "fa fa-cog fa-spin fa-2x"
            ]
        ]
    ]


let renderItems = function
    | NotStarted -> Html.div "Not started yet"

    | InProgress -> spinner

    | Resolved (Ok storyItems) ->
        React.fragment [ for item in storyItems do renderItem item ]

    | Resolved (Error error) ->
        renderError error


let render (state: State) dispatch =

    Html.div [
        prop.style [ style.padding 20 ]
        prop.children [
            Html.h1 [
                prop.className "title"
                prop.text "ElmishHackernews"
            ]

            renderTabs state.CurrentStory dispatch
            renderItems state.StoryItems
        ]
    ]




Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run