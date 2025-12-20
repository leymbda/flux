namespace Numberlink.ZigZag.Core

open Numberlink.ZigZag.Core.Lib
open System

type Coordinate =
    | Orthogonal of int * int

type TemplateVertex =
    | Unknown
    | Bridge

type Template = {
    Id: Guid
    Graph: Graph<TemplateVertex, unit>
    Layout: Map<Guid, Coordinate>
}

module Template =
    /// Create an empty template graph.
    let create id = {
        Id = id
        Graph = Graph.empty
        Layout = Map.empty
    }

    /// Add an uncomputed vertex to the template graph to generate levels off.
    let addVertex vertexId coordinate (template: Template) =
        let graph = Graph.addVertex vertexId Unknown template.Graph
        let layout = Map.add vertexId coordinate template.Layout

        { template with Graph = graph; Layout = layout }

    /// Add a bridge vertex to the template graph.
    let addBridge vertexId coordinate (template: Template) =
        let graph = Graph.addVertex vertexId Bridge template.Graph
        let layout = Map.add vertexId coordinate template.Layout
        
        { template with Graph = graph; Layout = layout }

    /// Add an edge between two vertices in the template graph.
    let addEdge fromVertexId toVertexId edgeId (template: Template) =
        let graph = Graph.addEdge fromVertexId toVertexId edgeId () template.Graph
        
        { template with Graph = graph }

    /// Validate the template to ensure basic requirements to generate levels from it are met.
    let validate (template: Template) =
        raise<bool> (System.NotImplementedException()) // TODO: Implement
