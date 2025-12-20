namespace Numberlink.ZigZag.Core.Lib

open System

type AdjacencyList = {
    Relations: Map<Guid, Map<Guid, Guid>>
    EdgePairs: Map<Guid, Guid * Guid>
}

module AdjacencyList =
    /// Create an empty adjacency list.
    let empty =
        let relations = Map.empty
        let edgePairs = Map.empty

        { Relations = relations; EdgePairs = edgePairs }

    /// Get the neighbors of a vertex by its ID, or an empty map if the vertex does not exist.
    let get (id: Guid) (adjacencyList: AdjacencyList) =
        Map.tryFind id adjacencyList.Relations |> Option.defaultValue Map.empty

    /// Bidirectionally link two vertices in the adjacency list with the given edge.
    let link (fromId: Guid) (toId: Guid) (edgeId: Guid) (adjacencyList: AdjacencyList): AdjacencyList =
        let addLink vertexId1 vertexId2 edgeId al =
            let neighbors =
                get vertexId1 al
                |> Map.add vertexId2 edgeId

            let relations = Map.add vertexId1 neighbors al.Relations
            let edgePairs = Map.add edgeId (vertexId1, vertexId2) al.EdgePairs

            { al with Relations = relations; EdgePairs = edgePairs }

        adjacencyList
        |> addLink fromId toId edgeId
        |> addLink toId fromId edgeId

    /// Bidirectionally unlink two vertices in the adjacency list.
    let unlink (fromId: Guid) (toId: Guid) (adjacencyList: AdjacencyList): AdjacencyList =
        let removeLink vertexId1 vertexId2 al =
            let neighbors =
                get vertexId1 al
                |> Map.remove vertexId2

            let relations =
                if Map.isEmpty neighbors then Map.remove vertexId1 al.Relations
                else Map.add vertexId1 neighbors al.Relations

            let edgePairs =
                al.Relations
                |> Map.tryFind vertexId1
                |> Option.bind (Map.tryFind vertexId2)
                |> Option.map (fun edgeId -> Map.remove edgeId al.EdgePairs)
                |> Option.defaultValue al.EdgePairs

            { al with Relations = relations; EdgePairs = edgePairs }

        adjacencyList
        |> removeLink fromId toId
        |> removeLink toId fromId

    /// Remove a vertex from all neighbors.
    let removeVertex (vertexId: Guid) (adjacencyList: AdjacencyList): AdjacencyList =
        get vertexId adjacencyList
        |> Map.fold (fun al neighborId _ -> unlink vertexId neighborId al) adjacencyList

    /// Remove an edge from the adjacency list.
    let removeEdge (edgeId: Guid) (adjacencyList: AdjacencyList): AdjacencyList =
        match Map.tryFind edgeId adjacencyList.EdgePairs with
        | Some (fromId, toId) -> unlink fromId toId adjacencyList
        | None -> adjacencyList

type Relation<'v, 'e> = {
    Vertex: 'v
    Edge: 'e
}

type Graph<'v, 'e> = {
    Vertices: Map<Guid, 'v>
    Edges: Map<Guid, 'e>
    AdjacencyList: AdjacencyList
}

module Graph =
    /// Create an empty graph.
    let empty<'v, 'e> =
        let vertices = Map.empty<Guid, 'v>
        let edges = Map.empty<Guid, 'e>
        let adjacencyList = AdjacencyList.empty
    
        { Vertices = vertices; Edges = edges; AdjacencyList = adjacencyList }

    /// Add a vertex to the graph.
    let addVertex (vertexId: Guid) (vertex: 'v) (graph: Graph<'v, 'e>) =
        { graph with Vertices = Map.add vertexId vertex graph.Vertices }
        
    /// Remove a vertex and its associated edges from the graph.
    let removeVertex (vertexId: Guid) (graph: Graph<'v, 'e>) =
        let edges =
            AdjacencyList.get vertexId graph.AdjacencyList
            |> Map.fold (fun edges _ edgeId -> Map.remove edgeId edges) graph.Edges

        let vertices = Map.remove vertexId graph.Vertices
    
        let adjacencyList =
            graph.AdjacencyList
            |> AdjacencyList.removeVertex vertexId

        { graph with Vertices = vertices; Edges = edges; AdjacencyList = adjacencyList }

    /// Add an edge to the graph, connecting two vertices.
    let addEdge (fromVertexId: Guid) (toVertexId: Guid) (edgeId: Guid) (edge: 'e) (graph: Graph<'v, 'e>) =
        let edges = Map.add edgeId edge graph.Edges
        let adjacencyList = AdjacencyList.link fromVertexId toVertexId edgeId graph.AdjacencyList
            
        { graph with Edges = edges; AdjacencyList = adjacencyList }

    /// Remove an edge from the graph.
    let removeEdge (edgeId: Guid) (graph: Graph<'v, 'e>) =
        let edges = Map.remove edgeId graph.Edges
        let adjacencyList = AdjacencyList.removeEdge edgeId graph.AdjacencyList
            
        { graph with Edges = edges; AdjacencyList = adjacencyList }

    /// Get a vertex by its ID.
    let getVertex (vertexId: Guid) (graph: Graph<'v, 'e>) =
        Map.tryFind vertexId graph.Vertices

    /// Get an edge by its ID.
    let getEdge (edgeId: Guid) (graph: Graph<'v, 'e>) =
        Map.tryFind edgeId graph.Edges

    /// Get the relations of neighbors of a vertex by its ID, or an empty seq if the vertex does not exist.
    let getNeighbors (vertexId: Guid) (graph: Graph<'v, 'e>) =
        AdjacencyList.get vertexId graph.AdjacencyList
        |> Map.toSeq
        |> Seq.choose (fun (_, edgeId) ->
            match getEdge edgeId graph, getVertex vertexId graph with
            | Some edge, Some vertex -> Some { Vertex = vertex; Edge = edge }
            | _ -> None
        )

    /// Map over the vertices of the graph.
    let mapVertices (f: 'v -> 'u) (graph: Graph<'v, 'e>) =
        let vertices = Map.map (fun _ v -> f v) graph.Vertices
        let edges = graph.Edges
        let adjacencyList = graph.AdjacencyList

        { Vertices = vertices; Edges = edges; AdjacencyList = adjacencyList }

    /// Map over the edges of the graph.
    let mapEdges (f: 'e -> 'u) (graph: Graph<'v, 'e>) =
        let vertices = graph.Vertices
        let edges = Map.map (fun _ e -> f e) graph.Edges
        let adjacencyList = graph.AdjacencyList

        { Vertices = vertices; Edges = edges; AdjacencyList = adjacencyList }
