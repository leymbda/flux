namespace Numberlink.ZigZag.Core

open FsToolkit.ErrorHandling
open Numberlink.ZigZag.Core.Lib

type TemplateVertexPosition =
    | Orthogonal of x: int * y: int

module TemplateVertexPosition =
    /// Check if two vertex positions are of the same type.
    let isSameType pos1 pos2 =
        match pos1, pos2 with
        | Orthogonal _, Orthogonal _ -> true
        //| _ -> false

type TemplateVertexType =
    | Unobserved
    | Bridge

type TemplateVertex = {
    Type: TemplateVertexType
    Position: TemplateVertexPosition
}

type TemplateEdgeType =
    | Path
    | Warp

type TemplateEdge = {
    Type: TemplateEdgeType
}

type Template = {
    Graph: Graph<TemplateVertex, TemplateEdge>
}

module Template =
    /// Create an empty template.
    let empty =
        { Graph = Graph.empty }

    /// Add an unobserved vertex to the template graph to generate levels off.
    let addUnobserved vertexId position (template: Template) =
        let vertex = { Type = Unobserved; Position = position }
        let graph = Graph.addVertex vertexId vertex template.Graph

        { template with Graph = graph }

    /// Add a bridge vertex to the template graph.
    let addBridge vertexId position (template: Template) =
        let vertex = { Type = Bridge; Position = position }
        let graph = Graph.addVertex vertexId vertex template.Graph
        
        { template with Graph = graph }
        
    /// Remove any type of vertex from the template graph.
    let removeVertex vertexId (template: Template) =
        let graph = Graph.removeVertex vertexId template.Graph

        { template with Graph = graph }

    /// Add a possible path edge between two vertices in the template graph.
    let addPath fromVertexId toVertexId edgeId (template: Template) =
        let edge = { Type = Path }
        let graph = Graph.addEdge fromVertexId toVertexId edgeId edge template.Graph
        
        { template with Graph = graph }

    /// Add a warp edge between two vertices in the template graph.
    let addWarp fromVertexId toVertexId edgeId (template: Template) =
        let edge = { Type = Warp }
        let graph = Graph.addEdge fromVertexId toVertexId edgeId edge template.Graph
        
        { template with Graph = graph }

    /// Remove an edge from the template graph.
    let removeEdge edgeId (template: Template) =
        let graph = Graph.removeEdge edgeId template.Graph

        { template with Graph = graph }

    /// Validate the template for pre-generation requirements.
    let validate (template: Template) = result {
        // Ensure any vertices exist
        do!
            template.Graph
            |> Graph.getVertices
            |> Seq.isEmpty
            |> Result.requireFalse "Template must contain at least one vertex"

        // Ensure all vertices have the same position type
        let firstPosition =
            template.Graph
            |> Graph.getVertices
            |> Seq.head
            |> snd
            |> _.Position

        do!
            template.Graph
            |> Graph.getVertices
            |> Seq.forall (fun (_, v) -> TemplateVertexPosition.isSameType firstPosition v.Position)
            |> Result.requireTrue "All vertices must have the same position type"

        // Ensure edges connect existing vertices
        do!
            template.Graph.AdjacencyList.EdgePairs
            |> Map.values
            |> Seq.collect (fun v -> [fst v; snd v])
            |> Seq.distinct
            |> Seq.forall (fun vertexId -> Map.containsKey vertexId template.Graph.Vertices)
            |> Result.requireTrue "Adjacent list contains edges connecting non-existent vertices"

        // Ensure bridges have an even number of connections
        do!
            template.Graph.Vertices
            |> Map.toSeq
            |> Seq.filter (fun (_, vertex) -> vertex.Type = Bridge)
            |> Seq.forall (fun (vertexId, _) ->
                template.Graph
                |> Graph.getNeighbors vertexId
                |> Seq.length
                |> fun v -> v % 2 = 0
            )
            |> Result.requireTrue "Bridge vertices must have an even number of edges"

        // Ensure warps aren't stacked
        do!
            template.Graph
            |> Graph.getVertices
            |> Seq.forall (fun (vertexId, _) ->
                template.Graph
                |> Graph.getNeighbors vertexId
                |> Seq.filter (fun r -> r.Edge.Type = Warp)
                |> Seq.length
                |> (<=) 1
            )
            |> Result.requireTrue "Vertices cannot connect multiple warps"
    }
