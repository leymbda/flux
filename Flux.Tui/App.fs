module Flux.Tui.App

open Elmish
open Numberlink.ZigZag.Core
open Numberlink.ZigZag.Core.Lib
open System
open System.Threading

type Move =
    | Up
    | Down
    | Left
    | Right

type Model = {
    Level: Level
    Selected: bool
    Position: int * int // TODO: How to handle other types of coordinate systems?
    ConsoleLock: Lock
}

type Msg =
    | Render
    | Move of Move
    | ToggleSelect
    | Exit

let init level: Model * Cmd<Msg> =
    {
        Level = level
        Selected = false
        Position = 0, 0
        ConsoleLock = Lock()
    },
    Cmd.none

let update msg model: Model * Cmd<Msg> =
    let tryMove rx ry =
        let x = fst model.Position + rx
        let y = snd model.Position + ry

        let valid =
            model.Level.Template.Graph
            |> Graph.getVertices
            |> Seq.exists (fun (_, v) -> match v.Position with | Orthogonal(px, py) -> px = x && py = y)

        if valid then { model with Position = x, y }
        else model

    match msg with
    | Move Up -> tryMove 0 -1, Cmd.none
    | Move Down -> tryMove 0 1, Cmd.none
    | Move Left -> tryMove -1 0, Cmd.none
    | Move Right -> tryMove 1 0, Cmd.none

    | ToggleSelect ->
        { model with Selected = not model.Selected }, Cmd.none
        
    | Render -> model, Cmd.none
    | Exit -> model, Cmd.none

let view model dispatch: unit =
    lock model.ConsoleLock (fun () ->
        let coordinatesText = $"{fst model.Position}, {snd model.Position}"
        let selectedText = if model.Selected then "Selected" else "Not Selected"
        Console.Write($"\u001b[2K\r{coordinatesText} ({selectedText})")
    )

let subscribe model: Sub<Msg> =
    let controlHash =
        [
            model.Selected.ToString()
            model.Position |> fst |> _.ToString()
            model.Position |> snd |> _.ToString()
        ]
        |> String.concat ""

    [
        ["keystroke-sub"; controlHash],
        fun dispatch ->
            let cts = new CancellationTokenSource()
            let ct = cts.Token

            async {
                while not ct.IsCancellationRequested do
                    lock model.ConsoleLock (fun () ->
                        if Console.KeyAvailable then
                            let key = Console.ReadKey true |> _.Key

                            match key with
                            | k when k = ConsoleKey.Escape -> dispatch Exit
                            | k when k = ConsoleKey.UpArrow -> dispatch <| Move Up
                            | k when k = ConsoleKey.DownArrow -> dispatch <| Move Down
                            | k when k = ConsoleKey.LeftArrow -> dispatch <| Move Left
                            | k when k = ConsoleKey.RightArrow -> dispatch <| Move Right
                            | k when k = ConsoleKey.Spacebar -> dispatch ToggleSelect
                            | _ -> ()
                    )

                    do! Async.Sleep 33
            }
            |> Async.StartAsTask
            |> ignore

            { new IDisposable with member _.Dispose() = cts.Cancel() }
    ]

let program level =
    let exit = ref false

    Program.mkProgram init update view
    |> Program.withSubscription subscribe
    |> Program.withTermination ((=) Exit) (fun _ -> exit.Value <- true)
    |> Program.runWith level

    while not exit.Value do
        Thread.Sleep(250)
