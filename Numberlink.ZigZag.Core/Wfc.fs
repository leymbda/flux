namespace Numberlink.ZigZag.Core

open System

[<RequireQualifiedAccess>]
type NodePossibleState =
    | Terminal
    | Path

type UncollapsedNode = {
    Id: Guid
    PossibleStates: Map<NodePossibleState, float>
}

module UncollapsedNode =
    let [<Literal>] UNASSIGNED_COLOR = -1

    let private totalWeight possibleStates =
        possibleStates
        |> Map.fold (fun acc _ weight -> acc + weight) 0.0

    /// Create an uncollapsed node
    let create id possibleStates =
        { Id = id; PossibleStates = possibleStates }
        
    /// Calculate the entropy of the node where lower entropy means higher confidence
    let entropy (node: UncollapsedNode) =
        let totalPossibleStates = Map.count node.PossibleStates

        let mostConfidentWeight =
            node.PossibleStates
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.max

        let totalWeight = totalWeight node.PossibleStates

        let normalisedMostConfidentWeight =
            1.0 - mostConfidentWeight / totalWeight

        float totalPossibleStates + normalisedMostConfidentWeight

    /// Collapse the node into a definite state based on weights
    let collapse (random: Random) (node: UncollapsedNode) =
        let totalWeight = totalWeight node.PossibleStates
        let res = random.NextDouble() * totalWeight

        let state = 
            node.PossibleStates
            |> Map.toSeq
            |> Seq.reduce (fun (accState, accWeight) (curState, curWeight) ->
                if accWeight > 0.0 && accWeight - curWeight <= 0.0 then (curState, 0.0)
                else (accState, accWeight - curWeight)
            )
            |> fst

        match state with
        | NodePossibleState.Terminal -> Node.Terminal { Id = node.Id; Color = UNASSIGNED_COLOR }
        | NodePossibleState.Path -> Node.Path { Id = node.Id }

type WfcNode =
    | Uncollapsed of UncollapsedNode
    | Collapsed of Node

type WfcGraph = {
    Nodes: Map<Guid, WfcNode>
    Edges: Set<Edge>
}

module WfcGraph =
    /// Create a WFC graph from a template
    let create (template: TemplateGraph) =
        let nodes = Map.empty<Guid, WfcNode>

        // TODO: Map template nodes to WFC nodes with initial possible states (based on constraints/adjacency rules)

        { Nodes = nodes; Edges = template.Edges }

    /// Find the node with the lowest entropy, or None if all nodes are collapsed
    let tryFindLowestEntropyNode (graph: WfcGraph) =
        let uncollapsed =
            graph.Nodes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.collect (function | Uncollapsed u -> [u] | Collapsed _ -> [])

        if Seq.isEmpty uncollapsed then None
        else Seq.minBy UncollapsedNode.entropy uncollapsed |> Some

    /// Collapse the specified node and recursively collapse any subsequent determined nodes
    let rec propagate (random: Random) (node: UncollapsedNode) (graph: WfcGraph) =
        let collapsed = UncollapsedNode.collapse random node
 
        // TODO: Update edges and nodes impacted by the collapse
        // TODO: Recursively call to collapse any nodes that are now determined
            
        let nodes = graph.Nodes
        let edges = graph.Edges

        { graph with
            Nodes = Map.add node.Id (Collapsed collapsed) nodes
            Edges = edges }

    /// Recursively collapse the graph until all nodes are collapsed
    let rec collapse (random: Random) (graph: WfcGraph) =
        match tryFindLowestEntropyNode graph with
        | None -> graph
        | Some id ->
            let collapsedGraph = propagate random id graph
            collapse random collapsedGraph
