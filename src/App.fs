module App

// https://github.com/HackerNews/API

open Elmish
open Elmish.React
open Feliz

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
    { StoryItems = NotStarted }, Cmd.ofMsg (LoadStoryItems Started)




Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run