namespace scrwtp.Graphs

open System

module Graphs = 
    // Take an edge and switch the start and end vertices.
    let invert = function ((v1, v2), weight) -> ((v2, v1), weight)

    // Recognizer for deconstructing graphs in a transparent manner.
    let (|Graph|_|)  graph = 
        match graph with
        | Directed (v, e)   -> Some (v, e)
        | Undirected (v, e) -> Some (v, Set.union e (Set.map invert e))

    // Get the graph edges; uses the recognizer so for each undirected edge will get a pair of directed edges.
    let getEdges = function 
        | Graph (v, e) -> e
        | other -> failwithf "Not a graph: %A" other

    // Builds a weighted adjacency list from a set of edges.
    let buildAdjacencyList =
        getEdges
        >> Seq.map (fun ((v1, v2), weight) -> (v1, (v2, weight)))
        >> Seq.groupBy fst
        >> Seq.map (fun (key, vertex) ->
            key, vertex |> Seq.map snd |> Set.ofSeq)
        >> Map.ofSeq
        >> AdjacencyList.Weighted

    // Builds an adjacency matrix from a set of edges.
    let buildAdjacencyMatrix = 
        getEdges 
        >> Map.ofSeq
        >> AdjacencyMatrix.Sparse

    // Gets the neighbour vertices using an adjacency list.
    let neighbours adjacencyList vertex = 
        match adjacencyList with
        | Simple list -> 
            match list |> Map.tryFind vertex with
            | Some vertices -> vertices |> Set.toList
            | None -> List.empty
        | Weighted list ->
            match list |> Map.tryFind vertex with
            | Some vertices -> vertices |> Set.toList |> List.map fst
            | None -> List.empty
                

        
        