namespace Flux.Tui.Lib

open System
open System.Drawing
open System.Threading

[<Struct>]
type TerminalPixel = {
    Foreground: Color
    Background: Color
}

module TerminalPixel =
    /// Create a terminal pixel from foreground and background colors.
    let create fg bg = { Foreground = fg; Background = bg }

    /// Convert a terminal pixel to ANSI representation for the terminal.
    let toString (pixel: TerminalPixel) =
        let fr = pixel.Foreground.R
        let fg = pixel.Foreground.G
        let fb = pixel.Foreground.B
        let br = pixel.Background.R
        let bg = pixel.Background.G
        let bb = pixel.Background.B

        $"\x1b[38;2;{fr};{fg};{fb}m\x1b[48;2;{br};{bg};{bb}m▀"

type TerminalRenderer = {
    Content: Color [,]
}

module TerminalRenderer =
    /// Create an empty terminal renderer.
    let empty =
        { Content = Array2D.empty }

    /// Update the content of the terminal renderer.
    let update content (renderer: TerminalRenderer) =
        { renderer with Content = content }

    /// Render the terminal content to the console, using the lock to prevent clashing console operations. Only renders
    /// rows that have been changed.
    let render (consoleLock: Lock) (terminal: TerminalRenderer) =
        lock consoleLock (fun () ->
            Console.SetCursorPosition(0, 0)

            Array2D.maprow id terminal.Content
            |> Array.prepend [| (Array.init (Array2D.length2 terminal.Content) (fun _ -> Color.FromArgb(0, 0, 0))) |]
            |> Array.mapFold (fun a c -> match a with | None -> None, Some c | Some p -> Some (p, c), None) None |> fst
            |> Array.choose id
            |> Array.map (fun (a, b) ->
                Array.init (Array2D.length2 terminal.Content) (fun i -> TerminalPixel.create (a.[i]) (b.[i]))
                |> Array.map TerminalPixel.toString
                |> String.concat ""
            )
            |> String.concat "\n"
            |> Console.Write
        )

// TODO: If renderer size is reduced, content outside new bounds needs to also be wiped
