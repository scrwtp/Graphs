namespace scrwtp.Graphs

open System

module Option =
    let defaultTo def opt = 
        match opt with
        | Some x    -> Some x
        | None      -> Some def

module GraphUtils = 
    let getWeight edge = 
        match edge with
        | Directed (_, _, weight)   -> weight
        | Undirected (_, _, weight) -> weight

    let buildSimpleAdjacencyList graph = 
        match graph with
        | G (vertices, edges) -> 
            edges
            |> Set.toSeq
            |> Seq.collect (fun edge ->
                match edge with
                | Directed (v1, v2, _) -> 
                    [ v1, v2 ]
                | Undirected (v1, v2, _) ->
                    [ (v1, v2); (v2, v1) ])
            |> Seq.groupBy fst
            |> Seq.map (fun (key, dst) ->
                key, dst |> Seq.map snd |> List.ofSeq)
            |> Map.ofSeq
            |> AdjacencyList.Simple

    let buildWeightedAdjacencyList getWeight graph = 
        match graph with
        | G (vertices, edges) -> 
            edges
            |> Set.toSeq
            |> Seq.collect (fun edge ->
                let weight = getWeight edge
                match edge with
                | Directed (v1, v2, _) -> 
                    [ v1, (v2, weight) ]
                | Undirected (v1, v2, _) ->
                    [ (v1, (v2, weight)); (v2, (v1, weight)) ])
            |> Seq.groupBy fst
            |> Seq.map (fun (key, dst) ->
                key, dst |> (Seq.map snd >> List.ofSeq))
            |> Map.ofSeq
            |> AdjacencyList.Weighted

    let buildAdjacencyList getWeight graph = 
        match graph with
        | G (vertices, edges) -> 
            edges
            |> Set.toSeq
            |> Seq.collect (fun edge ->
                let vertices = 
                    match edge with
                    | Directed (v1, v2, _) -> 
                        [ v1, v2 ]
                    | Undirected (v1, v2, _) ->
                        [ (v1, v2); (v2, v1) ]
                match getWeight with
                | Some weight -> vertices |> List.map (fun (v1, v2) -> (v1, (v2, weight)))
                | None        -> vertices)
            |> Seq.groupBy fst
            |> Seq.map (fun (key, dst) ->
                key, dst |> (Seq.map snd >> List.ofSeq))
            |> Map.ofSeq
            |> AdjacencyList.Weighted
                

        
        