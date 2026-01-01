namespace Numberlink.ZigZag.Core

open Numberlink.ZigZag.Core.Lib
open System

type Level = {
    Id: Guid
    Template: Template
    Links: Guid list list
}

module Level =
    /// Create an existing level with predefined links.
    let create levelId template links =
        { Id = levelId; Template = template; Links = links }

    /// Generate a new level based on the given template.
    let generate random levelId (template: Template) =
        Template.generate random template
        |> Result.map (fun collapsed ->
            collapsed
            |> Map.iter (fun vertexId state ->
                match state with
                | GeneratorDomain.Terminal ->
                    printfn "V %A: Terminal" vertexId

                | GeneratorDomain.Bridge edges ->
                    printfn "V %A: Bridge with %d edge pairs" vertexId (List.length edges)
                
                | GeneratorDomain.Path (edge1, edge2) ->
                    let neighbors =
                        Graph.getNeighbors vertexId template.Graph
                        |> Seq.filter (fun rel -> rel.EdgeId = edge1 || rel.EdgeId = edge2)
                        |> Seq.map _.VertexId
                        |> Seq.toArray

                    printfn "V %A: Path connecting %A" vertexId neighbors
            )

            // TODO: Remove above temporary debug output (or consider how to add this observability togglable)
            
            { Id = levelId; Template = template; Links = [] }
        )
        