module Flux.Tui.App

open Elmish
open Flux.Tui.Lib
open Numberlink.ZigZag.Core
open Numberlink.ZigZag.Core.Lib
open System
open System.Drawing
open System.Threading

type Move =
    | Up
    | Down
    | Left
    | Right

type Model = {
    TerminalRenderer: TerminalRenderer
    ConsoleLock: Lock
    Level: Level
    Selected: bool
    Position: int * int // TODO: How to handle other types of coordinate systems?
}

type Msg =
    | Render
    | Move of Move
    | ToggleSelect
    | Exit

let init level: Model * Cmd<Msg> =
    let lock = Lock()

    {
        TerminalRenderer = TerminalRenderer.empty
        ConsoleLock = lock
        Level = level
        Selected = false
        Position = 0, 0
    },
    Cmd.ofMsg Render

let update msg model: Model * Cmd<Msg> =
    let tryMove rx ry =
        let x = fst model.Position + rx
        let y = snd model.Position + ry

        let valid =
            model.Level.Template.Graph
            |> Graph.getVertices
            |> Seq.exists (fun (_, v) -> match v.Position with | Orthogonal(px, py) -> px = x && py = y)
        
        if valid then { model with Position = x, y }, Cmd.ofMsg Render
        else model, Cmd.none

    match msg with
    | Render ->
        let isCursor (x, y) =
            let px, py = model.Position
            px = x / 5 && py = y / 5

        let content =
            Array2D.init 15 15 (fun y x -> // TODO: Get size from level and multiply by 5
                if isCursor(x, y) then
                    if model.Selected then Color.FromArgb(255, 0, 0)
                    else Color.FromArgb(0, 0, 255)
                else
                    Color.FromArgb(127, 127, 127)
            )

        { model with TerminalRenderer = TerminalRenderer.update content model.TerminalRenderer }, Cmd.none

    | Move Up -> tryMove 0 -1
    | Move Down -> tryMove 0 1
    | Move Left -> tryMove -1 0
    | Move Right -> tryMove 1 0

    | ToggleSelect ->
        { model with Selected = not model.Selected }, Cmd.ofMsg Render
        
    | Exit -> model, Cmd.none

let view model _: unit =
    TerminalRenderer.render model.ConsoleLock model.TerminalRenderer

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
    Console.CursorVisible <- false

    let exit = ref false

    Program.mkProgram init update view
    |> Program.withSubscription subscribe
    |> Program.withTermination ((=) Exit) (fun _ -> exit.Value <- true)
    |> Program.runWith level

    while not exit.Value do
        Thread.Sleep(250)
