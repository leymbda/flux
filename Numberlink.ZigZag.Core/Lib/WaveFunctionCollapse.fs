namespace rec Numberlink.ZigZag.Core.Lib

open System

type Domain<'s when 's : comparison> = Map<'s, float>

module Domain =
    /// Calculate the total weight of a domain.
    let total (domain: Domain<'s>) =
        Map.fold (fun acc _ w -> acc + w) 0.0 domain

    /// Calculate the entropy of a domain.
    let entropy (domain: Domain<'s>) =
        if Map.isEmpty domain then
            0.0
        else
            let count = float (Map.count domain)
            let total = total domain
            let max = domain |> Map.toSeq |> Seq.map snd |> Seq.max
            let tie = 1.0 - (max / total)
            count + tie

    /// Observe a domain by drawing a single state according to weights. Returns None if the domain is empty.
    let observe (random: Random) (domain: Domain<'s>) =
        let total = total domain
        let pick = random.NextDouble() * total

        domain
        |> Map.toSeq
        |> Seq.fold
            (fun (accState, accWeight) (state, weight) ->
                if (accWeight > 0.0 && accWeight - weight <= 0.0) then (Some state, 0.0)
                else (accState, accWeight - weight)
            )
            (None, pick)
        |> fst

type Constraint<'v, 's, 'e, 'g when 's : comparison> = Guid -> Map<Guid, 's> -> Graph<'v, 'e> -> 'g -> float

type StepResult<'v, 's, 'e, 'g when 's : comparison> =
    | Contradiction
    | Progress of WaveFunctionCollapse<'v, 's, 'e, 'g>
    | Complete of WaveFunctionCollapse<'v, 's, 'e, 'g>

type WaveFunctionCollapse<'v, 's, 'e, 'g when 's : comparison> = {
    Random: Random
    Domains: Map<Guid, Domain<'s>>
    Collapsed: Map<Guid, 's>
    GlobalState: 'g
    Constraints: Constraint<'v, 's, 'e, 'g> list
    Graph: Graph<'v, 'e>
}

module WaveFunctionCollapse =
    /// Initialize a WFC model over a graph.
    let init random initialDomains initialGlobalState constraints (graph: Graph<'v, 'e>) = {
        Random = random;
        Domains = initialDomains graph;
        Collapsed = Map.empty;
        GlobalState = initialGlobalState graph;
        Constraints = constraints;
        Graph = graph
    }

    /// Get the ID of the uncollapsed vertex with the lowest entropy (with random tie-breaking), or None if all are
    /// collapsed.
    let tryPickLowestEntropy (wfc: WaveFunctionCollapse<'v, 's, 'e, 'g>) =
        let randomChoice seq =
            let i = wfc.Random.Next(Seq.length seq)
            Seq.item i seq

        wfc.Domains
        |> Map.toSeq
        |> Seq.filter (fun (vertexId, _) -> wfc.Collapsed.ContainsKey vertexId |> not)
        |> Seq.map (fun (vertexId, domain) -> vertexId, Domain.entropy domain)
        |> Seq.groupBy (fun (_, entropy) -> entropy)
        |> Seq.sortBy (fun (entropy, _) -> entropy)
        |> Seq.tryHead
        |> Option.map (snd >> randomChoice >> fst)

    /// Perform a single step of the WFC algorithm.
    let step (wfc: WaveFunctionCollapse<'v, 's, 'e, 'g>) =
        match tryPickLowestEntropy wfc with
        | None -> Complete wfc
        | Some vertexId ->
            match Map.find vertexId wfc.Domains with
            | d when Map.isEmpty d -> Contradiction
            | domain ->
                let propagate (initialWork: Map<Guid, 's>) =
                    // TODO: Propagate constraints and update domains accordingly
                    Progress wfc
                    
                match Domain.observe wfc.Random domain with
                | None -> Contradiction
                | Some state -> propagate (Map.empty |> Map.add vertexId state)

    /// Run the WFC algorithm to completion or contradiction.
    let run (wfc: WaveFunctionCollapse<'v, 's, 'e, 'g>) =
        // TODO: Initial propagation (collapse vertices that are already guaranteed)
        // TODO: Recursively step until complete or contradiction
        // TODO: Handle contradictions (backtracking)

        wfc.Collapsed
