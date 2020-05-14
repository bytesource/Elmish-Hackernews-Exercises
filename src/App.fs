module App

// https://github.com/HackerNews/API

open Elmish
open Elmish.React
open Feliz


Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run