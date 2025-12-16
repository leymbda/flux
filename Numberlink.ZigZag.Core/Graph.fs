namespace Numberlink.ZigZag.Core

open System

type Edge = {
    From: Guid
    To: Guid
}

type TerminalNode = {
    Id: Guid
    Color: int
}

type PathNode = {
   Id: Guid
}

type BridgeNode = {
    Id: Guid
}

type Node =
    | Terminal of TerminalNode
    | Path of PathNode
    | Bridge of BridgeNode

type Graph = {
    Nodes: Set<Node>
    Edges: Set<Edge>
}
