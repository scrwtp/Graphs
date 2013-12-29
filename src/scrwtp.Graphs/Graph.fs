namespace scrwtp.Graphs

open System

module Option =
    let defaultTo def opt = 
        match opt with
        | Some x    -> Some x
        | None      -> Some def

module Graphs = 
    let invert = function ((v1, v2), weight) -> ((v2, v1), weight)

    let (|Graph|_|)  graph = 
        match graph with
        | Directed (v, e)   -> Some (v, e)
        | Undirected (v, e) -> Some (v, Set.union e (Set.map invert e))

    let getEdges = function 
        | Graph (v, e) -> e
        | other -> failwithf "Not a graph: %A" other

    let buildAdjacencyList =
        getEdges
        >> Seq.map (fun ((v1, v2), weight) -> (v1, (v2, weight)))
        >> Seq.groupBy fst
        >> Seq.map (fun (key, vertex) ->
            key, vertex |> Seq.map snd |> Set.ofSeq)
        >> Map.ofSeq
        >> AdjacencyList.Weighted

    let buildAdjacencyMatrix = 
        getEdges 
        >> Map.ofSeq
        >> AdjacencyMatrix.Sparse
                

        
        