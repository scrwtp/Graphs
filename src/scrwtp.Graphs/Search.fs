﻿namespace scrwtp.Graphs

type Color = 
    | White
    | Gray
    | Black

module Search =
    module BreadthFirst = 
        ()

    module DepthFirst = 

        type VertexLookupInfo = 
            {
                predecessor: Vertex option
                discovered: int
                finished: int
            }
            static member Empty = 
                {
                    predecessor = None
                    discovered = 0
                    finished = 0
                }

        let buildLookup graph source =
            match graph with
            | Graphs.Graph (vertices, edges) -> 
                // start with the vertices colored white
                let colored = 
                    vertices
                    |> Seq.map (fun v -> v, Color.White)
                    |> Map.ofSeq

                // function that gets the neighbouring nodes reachable from a vertex
                let neighbours =
                    Graphs.neighbours (Graphs.buildAdjacencyList graph)

                // helper function for updating results lookup
                let update map vertex f = 
                    match map |> Map.tryFind vertex with
                    | Some info -> map |> Map.add vertex (f info)
                    | None      -> map |> Map.add vertex (f VertexLookupInfo.Empty)

                let rec visit current (initial, targets) (colored:Map<_,_>) (counter, lookup) =
                    match initial, targets with
                    // we have neighbouring vertices to visit
                    | _, v::vs      -> 
                        visit current 
                            (initial, vs) 
                            colored 
                            (counter, update lookup v <| fun info -> { info with predecessor = Some current })
                    // let's pick a new vertex to probe
                    | x::xs, []     -> 
                        match colored.[x] with
                        // we didn't see this vertex before; mark it gray and recursively visit it's neighbours
                        | Color.White -> 
                            visit x 
                                (x::xs, neighbours x) 
                                (colored |> Map.add x Color.Gray)  
                                (counter + 1, update lookup x <| fun info -> { info with discovered = counter + 1 })
                        // we've been here; mark it black and remove it from the list
                        | Color.Gray  -> 
                            visit x 
                                (xs, [])              
                                (colored |> Map.add x Color.Black) 
                                (counter + 1, update lookup x <| fun info -> { info with finished = counter + 1 })
                        // only for completeness sake; if this happens, something is very wrong with the algorithm
                        | Color.Black -> 
                            failwithf "Unexpected Black vertex: %A" x
                    // we're done, no more vertices to check
                    | [], []        -> lookup

                // kickstart with marking source as gray - as if the second pattern executed initially for source
                visit source 
                    (Set.toList vertices, neighbours source) 
                    (colored |> Map.add source Color.Gray) 
                    (1, update Map.empty source <| fun info -> { info with discovered = 1 })

            | other -> failwithf "Not a graph: %A" other
