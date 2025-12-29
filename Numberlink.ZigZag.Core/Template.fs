namespace Numberlink.ZigZag.Core

open FsToolkit.ErrorHandling
open Numberlink.ZigZag.Core.Lib
open System

type GeneratorState = {
    /// The weight for probability of a vertex being a terminal instead of path.
    TerminalWeight: float
}

type GeneratorDomain =
    | Terminal
    | Path of edges: Guid * Guid
    | Bridge of edges: (Guid * Guid) list

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
    BridgeIncrement: int option
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

    /// Add a bridge vertex to the template graph along with the edges it connects, defined as a list of tuples where
    /// the first item is the ID of the edge to create and the second is the ID of the vertex to connect to. The tuples
    /// are grouped in pairs to define the connections for each bridge segment.
    let addBridge vertexId (edges: ((Guid * Guid) * (Guid * Guid)) list) position (template: Template) =
        let vertex = { Type = Bridge; Position = position }

        let graph =
            Graph.addVertex vertexId vertex template.Graph
            |> List.foldBacki
                (fun i ((e1, v1), (e2, v2)) graph ->
                    graph
                    |> Graph.addEdge vertexId v1 e1 { Type = Path; BridgeIncrement = Some i }
                    |> Graph.addEdge vertexId v2 e2 { Type = Path; BridgeIncrement = Some i }
                )
                edges
        
        { template with Graph = graph }
        
    /// Remove any type of vertex from the template graph.
    let removeVertex vertexId (template: Template) =
        let graph = Graph.removeVertex vertexId template.Graph

        { template with Graph = graph }

    /// Add a possible path edge between two vertices in the template graph.
    let addPath fromVertexId toVertexId edgeId (template: Template) =
        let edge = { Type = Path; BridgeIncrement = None }
        let graph = Graph.addEdge fromVertexId toVertexId edgeId edge template.Graph
        
        { template with Graph = graph }

    /// Add a warp edge between two vertices in the template graph.
    let addWarp fromVertexId toVertexId edgeId (template: Template) =
        let edge = { Type = Warp; BridgeIncrement = None }
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

        // TODO: Ensure bridge edges are properly formed with pairs (probably integrate into above check)

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

    /// Generate a level from the template using Wave Function Collapse.
    let generate random (template: Template) =
        let initialDomains (graph: Graph<TemplateVertex, TemplateEdge>): Map<Guid, Domain<GeneratorDomain>> =
            graph
            |> Graph.getVertices
            |> Seq.choose (fun (vertexId, v) ->
                match v.Type with
                | Unobserved -> None
                | Bridge ->
                    graph
                    |> Graph.getNeighbors vertexId
                    |> Seq.filter _.Edge.BridgeIncrement.IsSome
                    |> Seq.groupBy (_.Edge.BridgeIncrement >> Option.get)
                    |> Seq.choose (fun (_, edges) ->
                        if Seq.length edges <> 2 then None
                        else Some (Seq.head edges |> _.EdgeId, Seq.last edges |> _.EdgeId)
                    )
                    |> Seq.toList
                    |> fun edges -> Some (vertexId, Map.singleton (GeneratorDomain.Bridge edges) 1.0)
            )
            |> Map.ofSeq

        let initialGlobalState (g: Graph<TemplateVertex, TemplateEdge>): GeneratorState =
            { TerminalWeight = 1.0 / 6.0 }

        let constraints: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> list = [
            fun vertexId domain graph state -> 1.0

            // TODO: How constraints currently work may be problematic. Need to investigate more before implementing
        ]

        let collapsed =
            template.Graph
            |> WaveFunctionCollapse.init random initialDomains initialGlobalState constraints
            |> WaveFunctionCollapse.run

        // TODO: Handle failure to collapse

        [] // TODO: Return links calculated from collapsed result
