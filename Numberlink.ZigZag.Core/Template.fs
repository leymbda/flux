namespace Numberlink.ZigZag.Core

open Numberlink.ZigZag.Core.Lib
open System

type TemplateVertex =
    | Unknown
    | Bridge

type Template = {
    Id: Guid
    Graph: Graph<TemplateVertex, unit>
}

module Template =
    /// Create an empty template graph.
    let create id = {
        Id = id
        Graph = Graph.empty
    }

    /// Add an uncomputed vertex to the template graph to generate levels off.
    let addVertex (vertexId: Guid) (template: Template) =
        let graph = Graph.addVertex vertexId Unknown template.Graph

        { template with Graph = graph }

    /// Add a bridge vertex to the template graph.
    let addBridge (vertexId: Guid) (template: Template) =
        let graph = Graph.addVertex vertexId Bridge template.Graph
        
        { template with Graph = graph }

    /// Add an edge between two vertices in the template graph.
    let addEdge (fromVertexId: Guid) (toVertexId: Guid) (edgeId: Guid) (template: Template) =
        let graph = Graph.addEdge fromVertexId toVertexId edgeId () template.Graph
        
        { template with Graph = graph }

    /// Validate the template to ensure basic requirements to generate levels from it are met.
    let validate (template: Template) =
        raise<bool> (System.NotImplementedException()) // TODO: Implement

// TODO: Figure out how to represent how the template looks visually and add to template type
