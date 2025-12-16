namespace Numberlink.Core

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
    Ids: Set<Guid>
}

type Node =
    | Terminal of TerminalNode
    | Path of PathNode
    | Bridge of BridgeNode

type Graph = {
    Nodes: Set<Node>
    Edges: Set<Edge>
}
