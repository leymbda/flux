open Numberlink.ZigZag.Core
open System

let seed = (new Random()).Next()
let random = new Random(seed)

let vertex1 = Guid.generate random
let vertex2 = Guid.generate random
let vertex3 = Guid.generate random
let vertex4 = Guid.generate random
let vertex6 = Guid.generate random
let vertex7 = Guid.generate random
let vertex8 = Guid.generate random
let vertex9 = Guid.generate random
let edge1 = Guid.generate random
let edge2 = Guid.generate random
let edge3 = Guid.generate random
let edge4 = Guid.generate random
let edge5 = Guid.generate random
let edge6 = Guid.generate random
let edge7 = Guid.generate random
let edge8 = Guid.generate random

let template = // 3x3 donut shape
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

for _ in 0..9 do Console.WriteLine()

let level =
    Level.generate random (Guid.generate random) template
    |> Result.defaultWith (fun err ->
        printfn "Error generating level: %s" err
        exit 1
    )

level.Template.Graph.Vertices
|> Map.iter (fun vertexId vertex ->
    match vertex.Position with
    | Orthogonal (x, y) ->
        let position = sprintf "(%d, %d)" x y
        printfn "  Vertex %A position %s" vertexId position
)
