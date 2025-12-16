namespace Numberlink.Core

open FsToolkit.ErrorHandling
open System

type GraphBuilder = {
    Nodes: Set<Node>
    Edges: Set<Edge>
}

module GraphBuilder =
    /// Create an empty graph builder
    let init () =
        { Nodes = Set.empty; Edges = Set.empty }

    /// Add a terminal node to the builder
    let addTerminal (color: int) (builder: GraphBuilder) =
        let node = Terminal { Id = Guid.NewGuid(); Color = color }
        { builder with Nodes = Set.add node builder.Nodes }

    /// Add a path node to the builder
    let addPath (builder: GraphBuilder) =
        let node = Path { Id = Guid.NewGuid() }
        { builder with Nodes = Set.add node builder.Nodes }

    /// Add a bridge node to the builder
    let addBridge (count: int) (builder: GraphBuilder) =
        let ids = seq { 1..count } |> Seq.map (fun _ -> Guid.NewGuid()) |> Set.ofSeq
        let node = Bridge { Ids = ids }
        { builder with Nodes = Set.add node builder.Nodes }

    /// Add an edge between two nodes in the builder, normalising to prevent inversions
    let addEdge (fromId: Guid) (toId: Guid) (builder: GraphBuilder) =
        let ids = [fromId; toId] |> List.sort
        let edge = { From = List.head ids; To = List.last ids }
        { builder with Edges = Set.add edge builder.Edges }

    /// Construct graph from the builder with validation
    let tryCreate (builder: GraphBuilder): Result<Graph, string> = result {
        // Ensure there are any nodes
        do!
            builder.Nodes
            |> Seq.isEmpty
            |> Result.requireFalse "Cannot accept creating empty graph"

        // Ensure edges do not loop back to the same node
        do!
            builder.Edges
            |> Seq.exists (fun e -> e.From = e.To)
            |> Result.requireFalse "Contains edges that loop back to the same node"

        // Ensure bridge nodes have at least 2 inner nodes
        do!
            builder.Nodes
            |> Seq.exists (function | Bridge b -> Set.count b.Ids < 2 | _ -> false)
            |> Result.requireFalse "Contains bridges with less than 2 inner nodes"

        // Ensure all edges connect existing nodes
        let nodeIds =
            builder.Nodes
            |> Seq.collect (function
                | Terminal t -> [t.Id]
                | Path p -> [p.Id]
                | Bridge b -> Set.toList b.Ids
            )
            |> Set.ofSeq

        do!
            builder.Edges
            |> Seq.forall (fun e -> Set.contains e.From nodeIds && Set.contains e.To nodeIds)
            |> Result.requireTrue "Contains edges that do not connect to valid nodes"
            
        // Ensure edges on bridges do not connect to themselves
        let builderIdGroups =
            builder.Nodes
            |> Seq.map (function | Bridge b -> Set.toList b.Ids | _ -> [])
            |> Seq.filter (List.isEmpty >> not)

        do!
            builder.Edges
            |> Seq.exists (fun e ->
                builderIdGroups
                |> Seq.exists (fun ids -> List.contains e.From ids && List.contains e.To ids)
            )
            |> Result.requireFalse "Contains edges that connect from a bridge back to itself"

        // Ensure all colors have two terminals
        do!
            builder.Nodes
            |> Seq.choose (function Terminal t -> Some t | _ -> None)
            |> Seq.groupBy _.Color
            |> Seq.exists (fun (_, ts) -> Seq.length ts <> 2)
            |> Result.requireFalse "Contains colors with incorrect number of terminals"

        // Ensure nodes have the expected number of edges
        let degreeOf id =
            builder.Edges
            |> Seq.filter (fun e -> e.From = id || e.To = id)
            |> Seq.length

        do!
            builder.Nodes
            |> Seq.exists (function
                | Terminal t -> degreeOf t.Id <> 1
                | Path p -> degreeOf p.Id <> 2
                | Bridge b -> b.Ids |> Set.exists (fun id -> degreeOf id <> 2)
            )
            |> Result.requireFalse "Contains nodes with incorrect number of edges"

        // Create graph on valid builder
        return { Nodes = builder.Nodes; Edges = builder.Edges }
    }
