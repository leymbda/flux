namespace Numberlink.ZigZag.Core.Lib

open System

type Domain<'s when 's : comparison> = Map<'s, float>

module Domain =
    let isEmpty domain =
        Map.isEmpty domain

    let count domain =
        Map.count domain

    let total domain =
        Map.fold (fun acc _ w -> acc + w) 0.0 domain

    let entropy domain =
        if isEmpty domain then None
        else
            domain
            |> Map.toSeq
            |> Seq.sumBy (fun (_, weight) ->
                let total = total domain
                let p = weight / total
                if p > 0.0 then -p * log p else 0.0)
            |> Some

    let isSingleton domain =
        count domain = 1

    let getSingleton domain =
        domain
        |> Map.toSeq
        |> Seq.head
        |> fst

    let trySingleton domain =
        if isSingleton domain then Some (getSingleton domain)
        else None

    let choicesByWeight domain =
        domain
        |> Map.toList
        |> List.sortByDescending snd
        |> List.map fst

    let observe (random: Random) domain =
        let rec pick threshold remaining =
            match remaining with
            | [] -> None
            | [(state, _)] -> Some state
            | (state, weight) :: rest ->
                if threshold <= weight then Some state
                else pick (threshold - weight) rest

        domain
        |> Map.toList
        |> pick (random.NextDouble() * total domain)

type Constraint<'v, 'e, 'g, 's when 's : comparison> =
    Guid -> Domain<'s> -> Map<Guid, 's> -> Graph<'v, 'e> -> 'g -> Domain<'s>

type WaveFunctionCollapse<'v, 's, 'e, 'g when 's : comparison> = {
    Random: Random
    Domains: Map<Guid, Domain<'s>>
    Collapsed: Map<Guid, 's>
    GlobalState: 'g
    Constraints: Constraint<'v, 'e, 'g, 's> list
    Graph: Graph<'v, 'e>
}

module WaveFunctionCollapse =
    let private getNeighborIds vertexId graph =
        AdjacencyList.getNeighbors vertexId graph.AdjacencyList
        |> Map.toList
        |> List.map fst

    let private isUncollapsed wfc vertexId =
        not (wfc.Collapsed.ContainsKey vertexId)

    let init random initialDomains initialGlobalState constraints graph = {
        Random = random
        Domains = initialDomains graph
        Collapsed = Map.empty
        GlobalState = initialGlobalState graph
        Constraints = constraints
        Graph = graph
    }

    let tryPickLowestEntropy wfc =
        let candidates =
            wfc.Domains
            |> Map.toList
            |> List.filter (fun (id, _) -> isUncollapsed wfc id)
            |> List.choose (fun (id, d) -> Domain.entropy d |> Option.map (fun e -> id, e))

        match candidates with
        | [] -> None
        | candidates ->
            let minEntropy =
                candidates
                |> List.map snd
                |> List.min

            let ties =
                candidates
                |> List.filter (fun (_, e) -> e = minEntropy)
                |> List.map fst

            Some (List.item (wfc.Random.Next(List.length ties)) ties)

    let applyConstraints vertexId wfc =
        wfc.Domains
        |> Map.tryFind vertexId
        |> Option.defaultValue Map.empty
        |> fun initialDomain ->
            wfc.Constraints
            |> List.fold (fun domain c -> c vertexId domain wfc.Collapsed wfc.Graph wfc.GlobalState) initialDomain
        |> Map.filter (fun _ w -> w > 0.0)

    let propagate newlyCollapsed wfc =
        let rec loop queue processed wfc =
            match queue with
            | [] ->
                Some wfc

            | id :: rest when Set.contains id processed || wfc.Collapsed.ContainsKey id ->
                loop rest processed wfc

            | id :: rest ->
                let domain = applyConstraints id wfc
                if Domain.isEmpty domain then None
                else
                    let processed = Set.add id processed

                    match Domain.trySingleton domain with
                    | Some state ->
                        let wfc = { wfc with
                                        Domains = Map.remove id wfc.Domains
                                        Collapsed = Map.add id state wfc.Collapsed }

                        let newWork =
                            getNeighborIds id wfc.Graph
                            |> List.filter (fun n -> isUncollapsed wfc n && not (Set.contains n processed))

                        loop (newWork @ rest) processed wfc

                    | None ->
                        let wfc = { wfc with Domains = Map.add id domain wfc.Domains }
                        loop rest processed wfc

        let initialWork =
            newlyCollapsed
            |> Map.toList
            |> List.collect (fun (id, _) -> getNeighborIds id wfc.Graph)

        let wfc = { wfc with Collapsed = Map.foldBack Map.add newlyCollapsed wfc.Collapsed }

        loop initialWork Set.empty wfc

    let collapseGuaranteed wfc =
        let guaranteed =
            wfc.Domains
            |> Map.toList
            |> List.choose (fun (id, d) ->
                if isUncollapsed wfc id then Domain.trySingleton d |> Option.map (fun s -> id, s)
                else None
            )
            |> Map.ofList

        if Map.isEmpty guaranteed then Some wfc
        else
            let wfc = { wfc with Domains = Map.filter (fun id _ -> not (Map.containsKey id guaranteed)) wfc.Domains }
            propagate guaranteed wfc

    let run wfc =
        let rec solve stack wfc =
            match tryPickLowestEntropy wfc with
            | None -> Some wfc.Collapsed
            | Some id ->
                match Map.tryFind id wfc.Domains with
                | Some domain when not (Domain.isEmpty domain) ->
                    tryChoices stack wfc id (Domain.choicesByWeight domain)
                | _ -> backtrack stack

        and tryChoices stack wfc id = function
            | [] -> backtrack stack
            | choice :: rest ->
                let newStack = if List.isEmpty rest then stack else (wfc, id, rest) :: stack
                match propagate (Map.ofList [ id, choice ]) wfc with
                | Some wfc -> solve newStack wfc
                | None -> tryChoices stack wfc id rest

        and backtrack = function
            | [] -> None
            | (wfc, id, choices) :: rest -> tryChoices rest wfc id choices

        collapseGuaranteed wfc
        |> Option.bind (solve [])
