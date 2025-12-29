open Flux.Tui
open Numberlink.ZigZag.Core
open System

let seed = 1;
let random = new Random(seed)

let vertex1 = Guid.NewGuid()
let vertex2 = Guid.NewGuid()
let vertex3 = Guid.NewGuid()
let vertex4 = Guid.NewGuid()
let vertex6 = Guid.NewGuid()
let vertex7 = Guid.NewGuid()
let vertex8 = Guid.NewGuid()
let vertex9 = Guid.NewGuid()
let edge1 = Guid.NewGuid()
let edge2 = Guid.NewGuid()
let edge3 = Guid.NewGuid()
let edge4 = Guid.NewGuid()
let edge5 = Guid.NewGuid()
let edge6 = Guid.NewGuid()
let edge7 = Guid.NewGuid()
let edge8 = Guid.NewGuid()

let template =
    Template.empty
    |> Template.addUnobserved vertex1 (Orthogonal(0, 0))
    |> Template.addUnobserved vertex2 (Orthogonal(0, 1))
    |> Template.addUnobserved vertex3 (Orthogonal(0, 2))
    |> Template.addUnobserved vertex4 (Orthogonal(1, 0))
    |> Template.addUnobserved vertex6 (Orthogonal(1, 2))
    |> Template.addUnobserved vertex7 (Orthogonal(2, 0))
    |> Template.addUnobserved vertex8 (Orthogonal(2, 1))
    |> Template.addUnobserved vertex9 (Orthogonal(2, 2))
    |> Template.addPath vertex1 vertex2 edge1
    |> Template.addPath vertex2 vertex3 edge2
    |> Template.addPath vertex3 vertex6 edge3
    |> Template.addPath vertex6 vertex9 edge4
    |> Template.addPath vertex9 vertex8 edge5
    |> Template.addPath vertex8 vertex7 edge6
    |> Template.addPath vertex7 vertex4 edge7
    |> Template.addPath vertex4 vertex1 edge8

    // 3x3 donut shape

let level =
    Level.generate random (Guid.NewGuid()) template
    |> fun l ->
        { l with
            Links = [
                [edge1; edge2; edge3; edge4]
                [edge5; edge6; edge7; edge8]
            ]
        }
        
    // Temporary until WFC implemented, tested, and working

App.program level
