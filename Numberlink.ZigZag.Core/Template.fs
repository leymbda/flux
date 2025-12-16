namespace Numberlink.ZigZag.Core

open FsToolkit.ErrorHandling
open System

type TemplateNode =
    | Uncollapsed of Guid
    | Bridge of Guid

type TemplateGraph = {
    Nodes: Map<Guid, TemplateNode>
    Edges: Set<Edge>
}

module TemplateGraph =
    /// Create an empty template graph
    let init () =
        { Nodes = Map.empty; Edges = Set.empty }

    let addUncollapsed (id: Guid) (graph: TemplateGraph) =
        { graph with Nodes = Map.add id (Uncollapsed id) graph.Nodes }
        
    /// Add a bridge node to the builder
    let addBridge (id: Guid) (graph: TemplateGraph) =
        { graph with Nodes = Map.add id (Bridge id) graph.Nodes }
        
    /// Add an edge between two nodes in the builder, normalising to prevent inversions
    let addEdge (fromId: Guid) (toId: Guid) (graph: TemplateGraph) =
        let ids = [fromId; toId] |> Seq.sort
        let edge = { From = Seq.head ids; To = Seq.last ids }
        { graph with Edges = Set.add edge graph.Edges }

    /// Construct template with validation
    let tryCreate (graph: TemplateGraph) = result {
        // Ensure there are any nodes and edges
        do!
            (Map.isEmpty graph.Nodes || Set.isEmpty graph.Edges)
            |> Result.requireFalse "Cannot accept creating empty graph"

        // Ensure edges do not loop back to the same node
        do!
            graph.Edges
            |> Seq.exists (fun e -> e.From = e.To)
            |> Result.requireFalse "Contains edges that loop back to the same node"

        // Ensure all edges connect existing nodes
        let nodeIds = graph.Nodes |> Map.keys

        do!
            graph.Edges
            |> Seq.forall (fun e -> Seq.contains e.From nodeIds && Seq.contains e.To nodeIds)
            |> Result.requireTrue "Contains edges that do not connect to valid nodes"

        // Ensure bridge nodes have an even number of inner nodes
        do!
            graph.Nodes
            |> Map.toSeq
            |> Seq.collect (fun (id, node) -> match node with | Bridge _ -> [id] | _ -> [])
            |> Seq.forall (fun id ->
                graph.Edges
                |> Seq.filter (fun e -> e.From = id || e.To = id)
                |> Seq.length
                |> (fun v -> v % 2 = 0)
            )
            |> Result.requireTrue "Contains bridges with an uneven number of edges"
    }
