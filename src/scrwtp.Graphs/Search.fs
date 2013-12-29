namespace scrwtp.Graphs

type Color = 
    | White
    | Gray
    | Black

module Search =
    let breadthFirst = ()

    let depthFirst graph source =
        match graph with
        | Graphs.Graph (vertices, edges) -> 
            let colored = 
                vertices
                |> Seq.map (fun v -> v, Color.White)
                |> Map.ofSeq

            let adjacencyList =
                Graphs.buildAdjacencyList graph

            let neighbours vertex = 
                match adjacencyList with
                | Simple list -> 
                    match list |> Map.tryFind vertex with
                    | Some vertices -> vertices |> Set.toList
                    | None -> List.empty
                | Weighted list ->
                    match list |> Map.tryFind vertex with
                    | Some vertices -> vertices |> Set.toList |> List.map fst
                    | None -> List.empty

            let rec visit (colored:Map<_,_>) (initial, targets) current counter (predecessors, discovered, finished) =
                match initial, targets with
                | _, v::vs      -> 
                    visit colored (initial, vs) current counter (predecessors |> Map.add v current, discovered, finished)
                | x::xs, []     -> 
                    match colored.[x] with
                    | Color.White -> visit (Map.add x Color.Gray colored) (x::xs, neighbours x) x (counter+1) (predecessors, Map.add x (counter+1) discovered, finished)
                    | Color.Gray  -> visit (Map.add x Color.Black colored) (xs, []) x (counter+1) (predecessors, discovered, Map.add x (counter+1) finished)
                    | Color.Black -> failwithf "Unexpected Black vertex: %A" x
                | [], []        -> predecessors, (discovered, finished)

            visit colored (Set.toList vertices, []) source 0 (Map.empty, Map.empty, Map.empty)

        | other -> failwithf "Not a graph: %A" other
