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
}


type Msg =
    | LoadStoryItems of AsyncStatus<Result<HackerNewsItem list, string>>


type State = {
    StoryItems: ValueStatus<Result<HackerNewsItem list, string>>
}

let init() =
    let initState = { StoryItems = NotStarted }
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
        Url = field.Optional.At ["url"] Decode.string
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


let fetchStoryItems =
    let url = "https://hacker-news.firebaseio.com/v0/topstories.json"

    async {
        let! status, responseText = Http.get url

        match status with 
        | 200 ->
            match parseHackerNewsIds responseText with
            | Ok (storyIds) ->
                let! storyItems =
                    storyIds
                    |> List.truncate 10
                    // |> List.map ( fun _ -> async { return Some { Id = 1; Title = "Test"; Url = Some "url" } })
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

        newState, Cmd.fromAsync fetchStoryItems

    | LoadStoryItems (Finished (Ok storyItems)) ->
        let newState = { state with StoryItems = Resolved (Ok storyItems)}

        newState, Cmd.none

    | LoadStoryItems (Finished (Error error)) ->
        let newState = { state with StoryItems = Resolved (Error error)}

        newState, Cmd.none


let renderError (errorMsg: string) = 
    Html.h1 [
        prop.style [ style.color.red ]
        prop.text errorMsg
    ]


let renderItem item =
    Html.div [
        prop.key item.Id
        prop.className "box"
        prop.style [ style.marginTop 15; style.marginBottom 15 ] 
        prop.children [
            match item.Url with 
            | Some url ->
                Html.a [
                    prop.style [ style.textDecoration.underline ]
                    prop.target.blank
                    prop.href url
                    prop.text item.Title
                ]

            | None ->
                Html.p item.Title

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

            renderItems state.StoryItems
        ]
    ]




Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run