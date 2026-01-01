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
    let generate random (template: Template) = result {
        let initialDomains (graph: Graph<TemplateVertex, TemplateEdge>): Map<Guid, Domain<GeneratorDomain>> =
            graph
            |> Graph.getVertices
            |> Seq.choose (fun (vertexId, v) ->
                match v.Type with
                | Unobserved ->
                    let edges =
                        graph
                        |> Graph.getNeighbors vertexId
                        |> Seq.map _.EdgeId
                        |> Seq.toList
                    
                    let pathDomains =
                        edges
                        |> List.allPairs edges
                        |> List.filter (fun (e1, e2) -> e1 < e2)
                        |> List.map (fun (e1, e2) -> GeneratorDomain.Path (e1, e2), 1.0)
                    
                    let domains =
                        (GeneratorDomain.Terminal, 1.0) :: pathDomains
                        |> Map.ofList
                    
                    Some (vertexId, domains)

                | Bridge ->
                    graph
                    |> Graph.getNeighbors vertexId
                    |> Seq.filter _.Edge.BridgeIncrement.IsSome
                    |> Seq.groupBy (_.Edge.BridgeIncrement >> Option.get)
                    |> Seq.choose (fun (_, edgeSeq) ->
                        if Seq.length edgeSeq <> 2 then None
                        else Some (Seq.head edgeSeq |> _.EdgeId, Seq.last edgeSeq |> _.EdgeId)
                    )
                    |> Seq.toList
                    |> fun bridgeEdges -> Some (vertexId, Map.singleton (GeneratorDomain.Bridge bridgeEdges) 1.0)

                    // TODO: Probably want the algorithm to fail if bridge edges aren't properly paired
            )
            |> Map.ofSeq

        let initialGlobalState (g: Graph<TemplateVertex, TemplateEdge>): GeneratorState =
            { TerminalWeight = 1.0 / 6.0 }

        let getUsedEdges domain =
            match domain with
            | GeneratorDomain.Terminal -> Set.empty
            | GeneratorDomain.Path (e1, e2) -> Set.ofList [e1; e2]
            | GeneratorDomain.Bridge edges -> edges |> List.collect (fun (e1, e2) -> [e1; e2]) |> Set.ofList

        // Rule: Two adjacent non-terminal vertices must agree on whether they use their connecting edge
        let edgeConsistencyConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                currentDomain
                |> Map.map (fun domain weight ->
                    let usedEdges = getUsedEdges domain
                    let isValid =
                        neighbors
                        |> List.forall (fun rel ->
                            let weUseEdge = Set.contains rel.EdgeId usedEdges
                            match Map.tryFind rel.VertexId collapsed with
                            | None -> true
                            | Some neighborDomain ->
                                match domain, neighborDomain with
                                | GeneratorDomain.Path _, GeneratorDomain.Path (ne1, ne2) ->
                                    let neighborUsesEdge = ne1 = rel.EdgeId || ne2 = rel.EdgeId
                                    weUseEdge = neighborUsesEdge
                                | GeneratorDomain.Path _, GeneratorDomain.Bridge nEdges ->
                                    let neighborUsesEdge = nEdges |> List.exists (fun (e1, e2) -> e1 = rel.EdgeId || e2 = rel.EdgeId)
                                    weUseEdge = neighborUsesEdge
                                | GeneratorDomain.Bridge myEdges, GeneratorDomain.Path (ne1, ne2) ->
                                    let weUseThisEdge = myEdges |> List.exists (fun (e1, e2) -> e1 = rel.EdgeId || e2 = rel.EdgeId)
                                    let neighborUsesEdge = ne1 = rel.EdgeId || ne2 = rel.EdgeId
                                    weUseThisEdge = neighborUsesEdge
                                | GeneratorDomain.Bridge myEdges, GeneratorDomain.Bridge nEdges ->
                                    let weUseThisEdge = myEdges |> List.exists (fun (e1, e2) -> e1 = rel.EdgeId || e2 = rel.EdgeId)
                                    let neighborUsesEdge = nEdges |> List.exists (fun (e1, e2) -> e1 = rel.EdgeId || e2 = rel.EdgeId)
                                    weUseThisEdge = neighborUsesEdge
                                | GeneratorDomain.Terminal, _ -> true
                                | _, GeneratorDomain.Terminal -> true
                        )
                    if isValid then weight else 0.0
                )

        // Rule: All terminal vertices must connect to exactly one path edge
        let terminalOnePathConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Terminal ->
                        let connectingPathCount =
                            neighbors
                            |> List.filter (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some (GeneratorDomain.Path (e1, e2)) -> e1 = rel.EdgeId || e2 = rel.EdgeId
                                | Some (GeneratorDomain.Bridge bridgeEdges) ->
                                    bridgeEdges |> List.exists (fun (e1, e2) -> e1 = rel.EdgeId || e2 = rel.EdgeId)
                                | _ -> false
                            )
                            |> List.length
                        
                        let uncollapsedCount =
                            neighbors
                            |> List.filter (fun rel -> not (Map.containsKey rel.VertexId collapsed))
                            |> List.length
                        
                        let neighborTerminalNeedsPath =
                            neighbors
                            |> List.exists (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some GeneratorDomain.Terminal ->
                                    let termNeighbors = Graph.getNeighbors rel.VertexId graph |> Seq.toList
                                    let termHasPath =
                                        termNeighbors
                                        |> List.exists (fun tn ->
                                            match Map.tryFind tn.VertexId collapsed with
                                            | Some (GeneratorDomain.Path (e1, e2)) -> e1 = tn.EdgeId || e2 = tn.EdgeId
                                            | _ -> false
                                        )
                                    not termHasPath
                                | _ -> false
                            )
                        
                        if neighborTerminalNeedsPath then 0.0
                        elif connectingPathCount > 1 then 0.0
                        elif connectingPathCount = 1 then weight
                        elif uncollapsedCount = 0 && connectingPathCount = 0 then 0.0
                        else weight
                    | _ -> weight
                )

        // Rule: A path cannot connect two terminals directly
        let pathMaxOneTerminalConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Path (e1, e2) ->
                        let getNeighborByEdge edgeId =
                            neighbors |> List.tryFind (fun rel -> rel.EdgeId = edgeId)
                        
                        let terminalCount =
                            [e1; e2]
                            |> List.choose getNeighborByEdge
                            |> List.filter (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some GeneratorDomain.Terminal -> true
                                | _ -> false
                            )
                            |> List.length
                        
                        if terminalCount > 1 then 0.0 else weight
                    | _ -> weight
                )

        // Rule: A bridge vertex cannot connect directly to a terminal
        let bridgeNoTerminalConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Terminal ->
                        let adjacentToBridge =
                            neighbors
                            |> List.exists (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some (GeneratorDomain.Bridge _) -> true
                                | _ -> false
                            )
                        if adjacentToBridge then 0.0 else weight
                    | GeneratorDomain.Bridge _ ->
                        let adjacentToTerminal =
                            neighbors
                            |> List.exists (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some GeneratorDomain.Terminal -> true
                                | _ -> false
                            )
                        if adjacentToTerminal then 0.0 else weight
                    | _ -> weight
                )

        // Rule: A warp edge cannot connect directly to a terminal or bridge
        let warpConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let warpNeighbors =
                    Graph.getNeighbors vertexId graph
                    |> Seq.filter (fun rel -> rel.Edge.Type = Warp)
                    |> Seq.toList
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Terminal when not warpNeighbors.IsEmpty -> 0.0
                    | GeneratorDomain.Bridge _ when not warpNeighbors.IsEmpty -> 0.0
                    | GeneratorDomain.Path (e1, e2) ->
                        let usedEdges = Set.ofList [e1; e2]
                        let badWarp =
                            warpNeighbors
                            |> List.exists (fun rel ->
                                Set.contains rel.EdgeId usedEdges &&
                                match Map.tryFind rel.VertexId collapsed with
                                | Some GeneratorDomain.Terminal -> true
                                | Some (GeneratorDomain.Bridge _) -> true
                                | _ -> false
                            )
                        if badWarp then 0.0 else weight
                    | _ -> weight
                )

        // Rule: All continuous paths must start and end at terminal vertices (no cycles)
        // Also: Two terminals connected by the same path chain is invalid (same line can't start and end at adjacent terminals)
        let noCyclesConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Path (e1, e2) ->
                        let rec trace visited currentVertex fromEdge =
                            if Set.contains currentVertex visited then Some (false, None) // Cycle detected
                            else
                                match Map.tryFind currentVertex collapsed with
                                | Some GeneratorDomain.Terminal -> Some (true, Some currentVertex) // Found terminal
                                | Some (GeneratorDomain.Path (pe1, pe2)) ->
                                    let nextEdge = if pe1 = fromEdge then pe2 else pe1
                                    graph
                                    |> Graph.getNeighbors currentVertex
                                    |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
                                    |> Option.bind (fun n -> trace (Set.add currentVertex visited) n.VertexId nextEdge)
                                | Some (GeneratorDomain.Bridge _) -> None
                                | None -> None
                        
                        let traceEdge edgeId =
                            graph
                            |> Graph.getNeighbors vertexId
                            |> Seq.tryFind (fun n -> n.EdgeId = edgeId)
                            |> Option.bind (fun n -> trace (Set.singleton vertexId) n.VertexId edgeId)
                        
                        let result1 = traceEdge e1
                        let result2 = traceEdge e2
                        
                        match result1, result2 with
                        | Some (false, _), _ | _, Some (false, _) -> 0.0 // Cycle detected
                        | Some (true, Some t1), Some (true, Some t2) when t1 = t2 -> 0.0 // Both ends lead to same terminal
                        | _ -> weight
                    
                    | GeneratorDomain.Terminal ->
                        // Check if any neighbor terminal can be reached through the path network
                        // If so, this terminal and that terminal would be on the same line
                        let neighborTerminals =
                            Graph.getNeighbors vertexId graph
                            |> Seq.choose (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some GeneratorDomain.Terminal -> Some rel.VertexId
                                | _ -> None
                            )
                            |> Set.ofSeq
                        
                        if Set.isEmpty neighborTerminals then weight
                        else
                            // Trace from each neighbor path to see if it reaches any of our neighbor terminals
                            let rec traceToTerminal visited currentVertex fromEdge =
                                if Set.contains currentVertex visited then None
                                else
                                    match Map.tryFind currentVertex collapsed with
                                    | Some GeneratorDomain.Terminal -> Some currentVertex
                                    | Some (GeneratorDomain.Path (pe1, pe2)) ->
                                        let nextEdge = if pe1 = fromEdge then pe2 else pe1
                                        graph
                                        |> Graph.getNeighbors currentVertex
                                        |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
                                        |> Option.bind (fun n -> traceToTerminal (Set.add currentVertex visited) n.VertexId nextEdge)
                                    | _ -> None
                            
                            let reachableTerminals =
                                Graph.getNeighbors vertexId graph
                                |> Seq.choose (fun rel ->
                                    match Map.tryFind rel.VertexId collapsed with
                                    | Some (GeneratorDomain.Path (pe1, pe2)) when pe1 = rel.EdgeId || pe2 = rel.EdgeId ->
                                        // This path connects to us, trace through it
                                        let nextEdge = if pe1 = rel.EdgeId then pe2 else pe1
                                        graph
                                        |> Graph.getNeighbors rel.VertexId
                                        |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
                                        |> Option.bind (fun n -> traceToTerminal (Set.ofList [vertexId; rel.VertexId]) n.VertexId nextEdge)
                                    | _ -> None
                                )
                                |> Set.ofSeq
                            
                            // If we can reach any of our neighbor terminals through paths, invalid
                            // This means the neighbor terminal and us are on the same line with an unused edge between us
                            if Set.intersect neighborTerminals reachableTerminals |> Set.isEmpty |> not then 0.0
                            else weight
                    
                    | _ -> weight
                )

        // Rule: A path connecting to a terminal must ensure that terminal doesn't already have another path
        let pathTerminalExclusiveConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Path (e1, e2) ->
                        let usedEdges = Set.ofList [e1; e2]
                        // Check each terminal neighbor - if we connect to it, it shouldn't already have a path
                        let wouldDoubleConnectTerminal =
                            neighbors
                            |> List.exists (fun rel ->
                                // We connect to this neighbor
                                Set.contains rel.EdgeId usedEdges &&
                                // Neighbor is a terminal
                                (match Map.tryFind rel.VertexId collapsed with
                                 | Some GeneratorDomain.Terminal ->
                                     // Check if terminal already has another path connecting to it
                                     let termNeighbors = Graph.getNeighbors rel.VertexId graph |> Seq.toList
                                     termNeighbors
                                     |> List.exists (fun tn ->
                                         tn.VertexId <> vertexId &&
                                         match Map.tryFind tn.VertexId collapsed with
                                         | Some (GeneratorDomain.Path (pe1, pe2)) ->
                                             pe1 = tn.EdgeId || e2 = tn.EdgeId
                                         | _ -> false
                                     )
                                 | _ -> false)
                            )
                        if wouldDoubleConnectTerminal then 0.0 else weight
                    | _ -> weight
                )

        // Rule: Continuous lines must only have path edges connecting parts of the path and terminals,
        // with no other edges connecting part of the path to itself (no shortcuts/loops)
        let noShortcutsConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                // Helper to trace the entire line from a starting point and collect all vertices on that line
                let rec collectLineVertices visited currentVertex fromEdgeOpt =
                    if Set.contains currentVertex visited then visited
                    else
                        let visited = Set.add currentVertex visited
                        match Map.tryFind currentVertex collapsed with
                        | Some GeneratorDomain.Terminal -> visited // Terminal is end of line
                        | Some (GeneratorDomain.Path (pe1, pe2)) ->
                            // Continue in both directions (or one if we came from somewhere)
                            let nextEdges = 
                                match fromEdgeOpt with
                                | Some fromEdge -> [if pe1 = fromEdge then pe2 else pe1]
                                | None -> [pe1; pe2]
                            nextEdges
                            |> List.fold (fun acc nextEdge ->
                                graph
                                |> Graph.getNeighbors currentVertex
                                |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
                                |> Option.map (fun n -> collectLineVertices acc n.VertexId (Some nextEdge))
                                |> Option.defaultValue acc
                            ) visited
                        | _ -> visited
                
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Terminal ->
                        // Find all vertices on the same line
                        // by tracing through connecting paths
                        let lineVertices =
                            neighbors
                            |> List.fold (fun acc rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some (GeneratorDomain.Path (pe1, pe2)) when pe1 = rel.EdgeId || pe2 = rel.EdgeId ->
                                    // This path connects to us, trace the whole line
                                    collectLineVertices acc rel.VertexId (Some rel.EdgeId)
                                | _ -> acc
                            ) (Set.singleton vertexId)
                        
                        // Check if any ADJACENT vertex (neighbor via ANY edge) is also on our line
                        // but connected via an edge that ISN'T part of the path
                        let hasShortcut =
                            neighbors
                            |> List.exists (fun rel ->
                                // Is this neighbor on our line?
                                let neighborOnLine = Set.contains rel.VertexId lineVertices
                                // Is the edge to them used by the path? (for terminals, no edges are "used")
                                let edgeUsedByNeighbor =
                                    match Map.tryFind rel.VertexId collapsed with
                                    | Some (GeneratorDomain.Path (pe1, pe2)) -> pe1 = rel.EdgeId || pe2 = rel.EdgeId
                                    | _ -> false // Terminals don't use edges
                                
                                // Shortcut = neighbor is on our line but edge isn't used
                                neighborOnLine && not edgeUsedByNeighbor
                            )
                        
                        if hasShortcut then 0.0 else weight
                    
                    | GeneratorDomain.Path (e1, e2) ->
                        let usedEdges = Set.ofList [e1; e2]
                        
                        // Collect all vertices on the same line
                        let lineVertices = collectLineVertices Set.empty vertexId None
                        
                        // Check if any neighbor via UNUSED edge is on the same line
                        let hasShortcut =
                            neighbors
                            |> List.exists (fun rel ->
                                // Edge is not used by this path
                                not (Set.contains rel.EdgeId usedEdges) &&
                                // But neighbor is on the same line
                                Set.contains rel.VertexId lineVertices
                            )
                        
                        if hasShortcut then 0.0 else weight
                    
                    | _ -> weight
                )

        // Rule: Two adjacent terminals that are both on the same line is invalid
        // This is simpler than full shortcut detection: just check if both terminals can be traced to each other
        let adjacentTerminalSameLineConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Terminal ->
                        // Check if we have an adjacent terminal
                        let adjacentTerminals =
                            neighbors
                            |> List.choose (fun rel ->
                                match Map.tryFind rel.VertexId collapsed with
                                | Some GeneratorDomain.Terminal -> Some rel
                                | _ -> None
                            )
                        
                        if List.isEmpty adjacentTerminals then weight
                        else
                            // For each adjacent terminal, check if we can reach it through the path network
                            // If we can, then we're on the same line and the direct edge is a shortcut
                            let rec traceToVertex target visited currentVertex fromEdge =
                                if currentVertex = target then true
                                elif Set.contains currentVertex visited then false
                                else
                                    match Map.tryFind currentVertex collapsed with
                                    | Some GeneratorDomain.Terminal -> false // Hit a different terminal
                                    | Some (GeneratorDomain.Path (pe1, pe2)) ->
                                        let nextEdge = if pe1 = fromEdge then pe2 else pe1
                                        graph
                                        |> Graph.getNeighbors currentVertex
                                        |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
                                        |> Option.map (fun n -> traceToVertex target (Set.add currentVertex visited) n.VertexId nextEdge)
                                        |> Option.defaultValue false
                                    | _ -> false
                        
                            // Check from each of our path neighbors if they can reach any adjacent terminal
                            let canReachAdjacentTerminal =
                                adjacentTerminals
                                |> List.exists (fun adjTerm ->
                                    // Try to reach this adjacent terminal through the path network
                                    neighbors
                                    |> List.exists (fun rel ->
                                        rel.VertexId <> adjTerm.VertexId && // Don't take the direct route
                                        match Map.tryFind rel.VertexId collapsed with
                                        | Some (GeneratorDomain.Path (pe1, pe2)) when pe1 = rel.EdgeId || pe2 = rel.EdgeId ->
                                            // This path connects to us, trace through it
                                            let nextEdge = if pe1 = rel.EdgeId then pe2 else pe1
                                            graph
                                            |> Graph.getNeighbors rel.VertexId
                                            |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
                                            |> Option.map (fun n -> traceToVertex adjTerm.VertexId (Set.ofList [vertexId; rel.VertexId]) n.VertexId nextEdge)
                                            |> Option.defaultValue false
                                        | _ -> false
                                    )
                                )
                        
                            if canReachAdjacentTerminal then 0.0 else weight
                    | _ -> weight
                )

        // Bias toward fewer terminals
        let terminalWeightConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph state ->
                currentDomain
                |> Map.map (fun domain weight ->
                    match domain with
                    | GeneratorDomain.Terminal -> weight * state.TerminalWeight
                    | _ -> weight
                )

        let constraints = [
            edgeConsistencyConstraint
            terminalOnePathConstraint
            pathMaxOneTerminalConstraint
            bridgeNoTerminalConstraint
            warpConstraint
            noCyclesConstraint
            pathTerminalExclusiveConstraint
            noShortcutsConstraint
            adjacentTerminalSameLineConstraint
            terminalWeightConstraint
        ]
        
        // TODO: Constraints do not work and are a failed claude attempt, need to be rewritten by hand
        
        let! collapsed =
            template.Graph
            |> WaveFunctionCollapse.init random initialDomains initialGlobalState constraints
            |> WaveFunctionCollapse.run
            |> Result.requireSome "Failed to generate level from template"

        return collapsed
    }
