namespace Numberlink.ZigZag.Core

open FsToolkit.ErrorHandling
open Numberlink.ZigZag.Core.Lib
open System

type GeneratorDomain =
    | Terminal of edge: Guid
    | Path of edges: Guid * Guid
    | Bridge of edges: (Guid * Guid) list

module GeneratorDomain =
    /// Get all edges used in a domain.
    let edges domain =
        match domain with
        | Terminal e -> seq { e }
        | Path (e1, e2) -> seq { e1; e2 }
        | Bridge edges -> edges |> List.collect (fun (e1, e2) -> [e1; e2]) |> List.toSeq

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

    // TODO: Should validation take place before generation rather than being separate function?

    /// Generate a solution for the template.
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
                        |> List.filter (fun (a, b) -> a < b)
                        |> List.map (fun (a, b) -> GeneratorDomain.Path (a, b), 1.0)

                    let terminalDomains =
                        edges
                        |> List.map (fun e -> GeneratorDomain.Terminal e, 1.0)
                    
                    let domains =
                        terminalDomains @ pathDomains
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

        /// Ensure vertices collapse into a compatible state with neighboring collapsed vertices
        let neighborConsistencyConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph ->
                let neighbors = Graph.getNeighbors vertexId graph

                let requiredEdges =
                    neighbors
                    |> Seq.collect (fun rel ->
                        Map.tryFind rel.VertexId collapsed
                        |> Option.map (GeneratorDomain.edges >> Seq.filter ((=) rel.EdgeId))
                        |> Option.defaultValue Seq.empty
                    )

                let optionalEdges = // TODO: Should this consider potential uncollapsed neighbor domains?
                    neighbors
                    |> Seq.collect (fun rel ->
                        Map.tryFind rel.VertexId collapsed
                        |> Option.map (fun _ -> Seq.empty)
                        |> Option.defaultValue (Seq.singleton rel.EdgeId)
                    )

                currentDomain
                |> Map.map (fun domain weight ->
                    /// Ensure neighboring edges are compatible with the current domain
                    let isEdgeConsistent =
                        match domain with
                        | GeneratorDomain.Terminal e ->
                            match Seq.toList requiredEdges, Seq.contains e optionalEdges with
                            | [], true -> true
                            | [r], _ when r = e -> true
                            | _, _ -> false

                        | GeneratorDomain.Path (e1, e2) ->
                            let hasUnexpectedRequired = Seq.exists (fun e -> e <> e1 && e <> e2) requiredEdges
                            let allowsEdge1 = Seq.contains e1 optionalEdges || Seq.contains e1 requiredEdges
                            let allowsEdge2 = Seq.contains e2 optionalEdges || Seq.contains e2 requiredEdges

                            not hasUnexpectedRequired && allowsEdge1 && allowsEdge2

                        | GeneratorDomain.Bridge edges ->
                            let bridgeEdges = edges |> Seq.collect (fun (e1, e2) -> [e1; e2])

                            let hasUnexpectedRequired =
                                Seq.exists (fun e -> not <| Seq.contains e bridgeEdges) requiredEdges

                            let allowsAllBridgeEdges =
                                bridgeEdges
                                |> Seq.forall (fun e -> Seq.contains e optionalEdges || Seq.contains e requiredEdges)

                            not hasUnexpectedRequired && allowsAllBridgeEdges

                    /// Ensure neighboring vertex types are compatible with the current domain
                    let isTypeConsistent =
                        neighbors
                        |> Seq.collect (fun rel ->
                            Map.tryFind rel.VertexId collapsed
                            |> Option.map (fun d -> [d])
                            |> Option.defaultValue []
                        )
                        |> Seq.forall (fun neighborDomain ->
                            match domain, neighborDomain with
                            | GeneratorDomain.Terminal _, GeneratorDomain.Bridge _ -> false
                            | GeneratorDomain.Bridge _, GeneratorDomain.Terminal _ -> false
                            | _, _ -> true
                        )

                    /// Ensure connected warps only connect compatible domains
                    let isWarpConsistent =
                        true
                        
                        // TODO: Warps need to have property in template to specify if they must be used in order to
                        //       constrain appropriately for requirements. For now, a level could have warps but none
                        //       be used. Once implemented, this check should ensure it so domains that don't use a
                        //       required warp are rejected.

                    if isEdgeConsistent && isTypeConsistent && isWarpConsistent then weight
                    else 0.0
                )
                |> Map.filter (fun _ weight -> weight <> 0.0)

        /// Ensure that all continuous lines end with terminals
        let lineTerminationConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph ->
                currentDomain // TODO: Implement

        /// Ensure continuous lines do not have available shortcuts
        let lineShortcutConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
            fun vertexId currentDomain collapsed graph ->
                currentDomain // TODO: Implement

        /// Multiply domain weights by multipliers defined in the generator state, proportional to other possible states
        let weightMultiplierConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
            fun _ currentDomain _ _ ->
                currentDomain // TODO: Implement

        // TODO: Add extra constraints as needed e.g. bridge reflection constraint

        let constraints = [
            neighborConsistencyConstraint
            lineTerminationConstraint
            lineShortcutConstraint
            weightMultiplierConstraint
        ]

        //// Rule: All continuous paths must start and end at terminal vertices (no cycles)
        //// Also: Two terminals connected by the same path chain is invalid (same line can't start and end at adjacent terminals)
        //let noCyclesConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
        //    fun vertexId currentDomain collapsed graph state ->
        //        currentDomain
        //        |> Map.map (fun domain weight ->
        //            match domain with
        //            | GeneratorDomain.Path (e1, e2) ->
        //                let rec trace visited currentVertex fromEdge =
        //                    if Set.contains currentVertex visited then Some (false, None) // Cycle detected
        //                    else
        //                        match Map.tryFind currentVertex collapsed with
        //                        | Some GeneratorDomain.Terminal -> Some (true, Some currentVertex) // Found terminal
        //                        | Some (GeneratorDomain.Path (pe1, pe2)) ->
        //                            let nextEdge = if pe1 = fromEdge then pe2 else pe1
        //                            graph
        //                            |> Graph.getNeighbors currentVertex
        //                            |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
        //                            |> Option.bind (fun n -> trace (Set.add currentVertex visited) n.VertexId nextEdge)
        //                        | Some (GeneratorDomain.Bridge _) -> None
        //                        | None -> None
                        
        //                let traceEdge edgeId =
        //                    graph
        //                    |> Graph.getNeighbors vertexId
        //                    |> Seq.tryFind (fun n -> n.EdgeId = edgeId)
        //                    |> Option.bind (fun n -> trace (Set.singleton vertexId) n.VertexId edgeId)
                        
        //                let result1 = traceEdge e1
        //                let result2 = traceEdge e2
                        
        //                match result1, result2 with
        //                | Some (false, _), _ | _, Some (false, _) -> 0.0 // Cycle detected
        //                | Some (true, Some t1), Some (true, Some t2) when t1 = t2 -> 0.0 // Both ends lead to same terminal
        //                | _ -> weight
                    
        //            | GeneratorDomain.Terminal ->
        //                // Check if any neighbor terminal can be reached through the path network
        //                // If so, this terminal and that terminal would be on the same line
        //                let neighborTerminals =
        //                    Graph.getNeighbors vertexId graph
        //                    |> Seq.choose (fun rel ->
        //                        match Map.tryFind rel.VertexId collapsed with
        //                        | Some GeneratorDomain.Terminal -> Some rel.VertexId
        //                        | _ -> None
        //                    )
        //                    |> Set.ofSeq
                        
        //                if Set.isEmpty neighborTerminals then weight
        //                else
        //                    // Trace from each neighbor path to see if it reaches any of our neighbor terminals
        //                    let rec traceToTerminal visited currentVertex fromEdge =
        //                        if Set.contains currentVertex visited then None
        //                        else
        //                            match Map.tryFind currentVertex collapsed with
        //                            | Some GeneratorDomain.Terminal -> Some currentVertex
        //                            | Some (GeneratorDomain.Path (pe1, pe2)) ->
        //                                let nextEdge = if pe1 = fromEdge then pe2 else pe1
        //                                graph
        //                                |> Graph.getNeighbors currentVertex
        //                                |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
        //                                |> Option.bind (fun n -> traceToTerminal (Set.add currentVertex visited) n.VertexId nextEdge)
        //                            | _ -> None
                            
        //                    let reachableTerminals =
        //                        Graph.getNeighbors vertexId graph
        //                        |> Seq.choose (fun rel ->
        //                            match Map.tryFind rel.VertexId collapsed with
        //                            | Some (GeneratorDomain.Path (pe1, pe2)) when pe1 = rel.EdgeId || pe2 = rel.EdgeId ->
        //                                // This path connects to us, trace through it
        //                                let nextEdge = if pe1 = rel.EdgeId then pe2 else pe1
        //                                graph
        //                                |> Graph.getNeighbors rel.VertexId
        //                                |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
        //                                |> Option.bind (fun n -> traceToTerminal (Set.ofList [vertexId; rel.VertexId]) n.VertexId nextEdge)
        //                            | _ -> None
        //                        )
        //                        |> Set.ofSeq
                            
        //                    // If we can reach any of our neighbor terminals through paths, invalid
        //                    // This means the neighbor terminal and us are on the same line with an unused edge between us
        //                    if Set.intersect neighborTerminals reachableTerminals |> Set.isEmpty |> not then 0.0
        //                    else weight
                    
        //            | _ -> weight
        //        )

        //// Rule: Continuous lines must only have path edges connecting parts of the path and terminals,
        //// with no other edges connecting part of the path to itself (no shortcuts/loops)
        //let noShortcutsConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorState, GeneratorDomain> =
        //    fun vertexId currentDomain collapsed graph state ->
        //        // Helper to trace the entire line from a starting point and collect all vertices on that line
        //        let rec collectLineVertices visited currentVertex fromEdgeOpt =
        //            if Set.contains currentVertex visited then visited
        //            else
        //                let visited = Set.add currentVertex visited
        //                match Map.tryFind currentVertex collapsed with
        //                | Some GeneratorDomain.Terminal -> visited // Terminal is end of line
        //                | Some (GeneratorDomain.Path (pe1, pe2)) ->
        //                    // Continue in both directions (or one if we came from somewhere)
        //                    let nextEdges = 
        //                        match fromEdgeOpt with
        //                        | Some fromEdge -> [if pe1 = fromEdge then pe2 else pe1]
        //                        | None -> [pe1; pe2]
        //                    nextEdges
        //                    |> List.fold (fun acc nextEdge ->
        //                        graph
        //                        |> Graph.getNeighbors currentVertex
        //                        |> Seq.tryFind (fun n -> n.EdgeId = nextEdge)
        //                        |> Option.map (fun n -> collectLineVertices acc n.VertexId (Some nextEdge))
        //                        |> Option.defaultValue acc
        //                    ) visited
        //                | _ -> visited
                
        //        let neighbors = Graph.getNeighbors vertexId graph |> Seq.toList
                
        //        currentDomain
        //        |> Map.map (fun domain weight ->
        //            match domain with
        //            | GeneratorDomain.Terminal ->
        //                // Find all vertices on the same line
        //                // by tracing through connecting paths
        //                let lineVertices =
        //                    neighbors
        //                    |> List.fold (fun acc rel ->
        //                        match Map.tryFind rel.VertexId collapsed with
        //                        | Some (GeneratorDomain.Path (pe1, pe2)) when pe1 = rel.EdgeId || pe2 = rel.EdgeId ->
        //                            // This path connects to us, trace the whole line
        //                            collectLineVertices acc rel.VertexId (Some rel.EdgeId)
        //                        | _ -> acc
        //                    ) (Set.singleton vertexId)
                        
        //                // Check if any ADJACENT vertex (neighbor via ANY edge) is also on our line
        //                // but connected via an edge that ISN'T part of the path
        //                let hasShortcut =
        //                    neighbors
        //                    |> List.exists (fun rel ->
        //                        // Is this neighbor on our line?
        //                        let neighborOnLine = Set.contains rel.VertexId lineVertices
        //                        // Is the edge to them used by the path? (for terminals, no edges are "used")
        //                        let edgeUsedByNeighbor =
        //                            match Map.tryFind rel.VertexId collapsed with
        //                            | Some (GeneratorDomain.Path (pe1, pe2)) -> pe1 = rel.EdgeId || pe2 = rel.EdgeId
        //                            | _ -> false // Terminals don't use edges
                                
        //                        // Shortcut = neighbor is on our line but edge isn't used
        //                        neighborOnLine && not edgeUsedByNeighbor
        //                    )
                        
        //                if hasShortcut then 0.0 else weight
                    
        //            | GeneratorDomain.Path (e1, e2) ->
        //                let usedEdges = Set.ofList [e1; e2]
                        
        //                // Collect all vertices on the same line
        //                let lineVertices = collectLineVertices Set.empty vertexId None
                        
        //                // Check if any neighbor via UNUSED edge is on the same line
        //                let hasShortcut =
        //                    neighbors
        //                    |> List.exists (fun rel ->
        //                        // Edge is not used by this path
        //                        not (Set.contains rel.EdgeId usedEdges) &&
        //                        // But neighbor is on the same line
        //                        Set.contains rel.VertexId lineVertices
        //                    )
                        
        //                if hasShortcut then 0.0 else weight
                    
        //            | _ -> weight
        //        )

        let! collapsed =
            template.Graph
            |> WaveFunctionCollapse.init random initialDomains constraints
            |> WaveFunctionCollapse.run
            |> Result.requireSome "Failed to generate level from template"

        return collapsed
    }
