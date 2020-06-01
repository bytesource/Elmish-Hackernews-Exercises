module App

// https://github.com/HackerNews/API

open System

open Elmish
open Elmish.React
open Feliz

open Fable.SimpleHttp
open Fable.DateFunctions
open Thoth.Json


type Deferred<'t> =
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
    Published: DateTime
}


[<RequireQualifiedAccess>]
type Story =
    | New 
    | Top 
    | Best 
    | Job 


type DeferredResult<'t> = Deferred<Result<'t, string>>


// type DeferredStoryItem = DeferredResult<HackerNewsItem>

// The "outer" asynchronous operation is responsible for loading 
// the IDs of the story items from the Hackernews API 
// and from there we initialize the "inner" asynchronous states for the story items. 
type State = {
    CurrentStory: Story
    StoryItems: DeferredResult<Map<int, DeferredResult<HackerNewsItem>>>
}


type Msg =
    | ChangeStory of Story
    | LoadStoryIdentifiers of AsyncStatus<Result<int list, string>>
    | LoadStoryItem of int * Result<HackerNewsItem, string> 
    // Why no async status?
    // Answer by the author:
    // Because we don't need to know whether the operation has started or not. 
    // As soon as the IDs of the story items are loaded, 
    // the asynchronous state of each item is Deferred.InProgress




let init() =
    let initState = { CurrentStory = Story.New; StoryItems = NotStarted }
    let initCommand = Cmd.ofMsg (LoadStoryIdentifiers Started)
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


// https://gist.github.com/FaguiCurtain/9d2d835758e845cdd3055ef2b2555fab#file-apitest-fsx-L12
let toDateTime (timestamp:int64) =
    let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
    start.AddSeconds(float timestamp).ToLocalTime()


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
        Published = (field.Required.At [ "time" ] Decode.int64) |> toDateTime
        // Published = field.Required.At [ "time" ] Decode.datetime
    })

 

let parseHackerNewsItem json =
    Decode.fromString hackerNewsItemDecoder json


let fetchStoryItem (id: int) =
    let url = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" id
    let rnd = Random()

    async {
        do! Async.Sleep (rnd.Next(1000, 1500))  // Simulating network latency
        let! status, responseText = Http.get url

        match status with 
        | 200 -> 
            match parseHackerNewsItem responseText with
            | Ok item -> return LoadStoryItem (id, (Ok item))
            | Error cause -> return LoadStoryItem (id, (Error cause))

        | _   -> return LoadStoryItem (id, (Error "Couldn't load post"))
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
                let storyItems =
                    storyIds
                    |> List.truncate 10

                return LoadStoryIdentifiers (Finished (Ok storyItems))

            | Error errorMsg ->
                // Could not parse the array of story IDs
                return LoadStoryIdentifiers (Finished (Error errorMsg))

        | _ ->
            // Non-OK response finishes with an error
            return LoadStoryIdentifiers (Finished (Error "Network error: Couldn't load stories"))
    }




// ============================================== Helpers - End


let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with 
    | LoadStoryIdentifiers Started ->
        let newState = { state with StoryItems = InProgress }

        newState, Cmd.fromAsync (fetchStoryItems state.CurrentStory)

    | LoadStoryIdentifiers (Finished (Ok storyIds)) ->
        let storiesMap = Map.ofList [ for id in storyIds do id, Deferred.InProgress ]

        let newState = { state with StoryItems = Resolved (Ok storiesMap)}

        newState, Cmd.batch [ for id in storyIds -> Cmd.fromAsync (fetchStoryItem id) ]

    | LoadStoryIdentifiers (Finished (Error error)) ->
        let newState = { state with StoryItems = Resolved (Error error)}

        newState, Cmd.none

    | ChangeStory story ->
        let newState = { state with CurrentStory = story; StoryItems = InProgress }

        newState, Cmd.fromAsync (fetchStoryItems story)

    | LoadStoryItem (id, (Ok item)) ->

        match state.StoryItems with
        | Resolved (Ok storiesMap) ->
            // Map.add if a key already exists, its value will be updated with the new value.
            let newStoriesMap =
                storiesMap |> Map.add id (Resolved (Ok item))

            let newState = { state with StoryItems = Resolved (Ok newStoriesMap) }
            newState, Cmd.none

        // State sink
        | _ -> state, Cmd.none

    | LoadStoryItem (id, (Error message)) ->
        match state.StoryItems with 
        | Resolved (Ok storiesMap) ->
            let newStoriesMap =
                storiesMap |> Map.add id (Resolved (Error message))

            let newState = { state with StoryItems = Resolved (Ok newStoriesMap) }
            newState, Cmd.none

        // state sink
        | _ -> state, Cmd.none

            



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


let formatDate(date:DateTime) =
    date.ToString("dd-MM-yy")


let formatTime (time:DateTime) =
    let padZero n = if n < 10 then sprintf "0%d" n else string n
    sprintf "%s:%s:%s" (padZero time.Hour) (padZero time.Minute) (padZero time.Second)

open Fable.Core.JsInterop

let renderItemContent item =
    Html.div [
        prop.key item.Id
        prop.className "box"
        prop.style [ style.marginTop 15; style.marginBottom 15 ] 
        prop.children [
            div [ "columns"; "is-mobile" ] [
                div [ "column" ] [
                    Html.div [
                        prop.className [ "icon" ]
                        prop.style [ style.marginLeft 20 ]
                        prop.children [
                            Html.i [ prop.className "fa fa-poll fa-2x" ]
                            Html.span [
                                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                                prop.text (sprintf "Score: %d" item.Score)
                            ]
                            Html.span [
                                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                                let pubDate = item.Published
                                // type IDistanceInWords implementations:
                                // https://github.com/Zaid-Ajaj/Fable.DateFunctions/blob/master/src/DateFns.fs#L40
                                // createEmpty<'T> is part of the Fable.Core.JsInterop module
                                let formatOptions = createEmpty<IDistanceInWordsOptions>
                                // IDistanceInWords actual usage in code:
                                // https://github.com/Zaid-Ajaj/Fable.DateFunctions/blob/master/app/View.fs#L79
                                prop.text (sprintf "%s ago" (pubDate.DistanceInWordsToNow(formatOptions)))
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


let renderStoryItem (itemId: int) storyItem =
    let renderedItem =
        match storyItem with
        | NotStarted -> Html.none
        | InProgress -> spinner
        | Resolved (Ok item) -> renderItemContent item
        | Resolved (Error cause) -> renderError cause

    Html.div [
        prop.key itemId
        prop.className "box"
        prop.style [ style.marginTop 15; style.marginBottom 15 ]
        prop.children [ renderedItem ]

    ]


// https://gist.github.com/FaguiCurtain/9d2d835758e845cdd3055ef2b2555fab#file-apitest-fsx-L20
let toUnixTimeSeconds (d:DateTime) : int64 = 
    let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
    int64 (d - start).TotalSeconds


let renderStoryItems items = 
    match items with
    | NotStarted -> Html.none

    | InProgress -> spinner

    | Resolved (Ok storyItems) ->
        storyItems 
        |> Map.toList
        // Sort: Successfully resolved items at the top, starting with the most recent post.
        |> List.sortByDescending (fun (_, item) -> 
            match item with
            | Resolved (Ok post) -> post.Published
            | _ -> DateTime.MinValue) 
        |> List.map (fun (id, item) -> renderStoryItem id item)
        |> Html.div

    | Resolved (Error error) ->
        renderError error


let title = 
    Html.h1 [
        prop.className "title"
        prop.text "Elmish Hacker News"
    ]


let render (state: State) dispatch =

    Html.div [
        prop.style [ style.padding 20 ]
        prop.children [
            title
            renderTabs state.CurrentStory dispatch
            renderStoryItems state.StoryItems 
        ]
    ]




Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run