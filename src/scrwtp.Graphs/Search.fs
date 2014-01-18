namespace scrwtp.Graphs

type Color = 
    | White
    | Gray
    | Black

module Search =
    module BreadthFirst = 

        type VertexLookupInfo = 
            {
                predecessor: Vertex option
                distance: int
            }
            static member Empty = 
                {
                    predecessor = None
                    distance = 0
                }

        let buildLookup graph source = 
            match graph with
            | Graphs.Graph (vertices, edges) -> 
                // start with the vertices colored white
                let initialColoring = 
                    vertices
                    |> Seq.map (fun v -> v, Color.White)
                    |> Map.ofSeq

                // function that gets the neighbouring nodes reachable from a vertex
                let neighbours =
                    Graphs.neighbours (Graphs.buildAdjacencyList graph)

                // mutable queue for storing vertices
                let queue = System.Collections.Generic.Queue<Vertex>()
                
                let rec visit (queue: System.Collections.Generic.Queue<_>) vertexColoring (resultLookup: ResultLookup<_>) = 
                    // while queue is not empty...
                    if queue.Count = 0
                        then resultLookup.Unwrap
                        else
                            // ...grab a vertex and its info
                            let current = queue.Dequeue()
                            let currentInfo = resultLookup.FindOrEmpty(current)
                            
                            // fold over its neighbours, updating results lookup
                            let updatedColored, updatedLookup =
                                neighbours current
                                |> List.fold (fun (coloring : Map<Vertex, _>, lookup : ResultLookup<_>) vertex -> 
                                    match coloring.[vertex] with
                                    | Color.White ->
                                        // we haven't seen the vertex, put it in the queue and update its info
                                        queue.Enqueue vertex
                                        let updatedLookup =
                                            lookup.Update vertex <| fun info -> 
                                                { info with predecessor = Some current; distance = currentInfo.distance + 1 }
                                        let updatedColored = 
                                            coloring |> Map.add vertex Color.Gray
                                        (updatedColored, updatedLookup)
                                    | _ -> (coloring, lookup)) (vertexColoring, resultLookup)
                            
                            // try processing another vertex from the queue
                            visit queue updatedColored updatedLookup

                // put the starting vertex in the queue...
                queue.Enqueue source

                // ...and kick-off
                visit queue initialColoring (ResultLookup(Map.empty, VertexLookupInfo.Empty))

            | other -> failwithf "Not a graph: %A" other


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
                let initialColoring = 
                    vertices
                    |> Seq.map (fun v -> v, Color.White)
                    |> Map.ofSeq

                // function that gets the neighbouring nodes reachable from a vertex
                let neighbours =
                    Graphs.neighbours (Graphs.buildAdjacencyList graph)

                let rec visit current counter (vertexColoring : Map<_,_>, resultLookup : ResultLookup<_>) = 
                    // it's a new vertex; tag it as discovered and mark it gray
                    let initial = 
                        (vertexColoring |> Map.add current Color.Gray,
                         (resultLookup.Update current <| fun info -> { info with discovered = counter + 1 }),
                         counter)

                    // fold over its neighbours, updating results lookup
                    neighbours current 
                    |> List.fold (fun (coloring : Map<_,_>, lookup : ResultLookup<_>, counter) vertex ->
                        match coloring.[vertex] with
                        | Color.White -> 
                            // we haven't seen the vertex; visit it recursively
                            visit vertex counter (coloring, lookup.Update vertex <| fun info -> { info with predecessor = Some current })
                        | _ -> (coloring, lookup, counter)) initial
                    // mark the current vertex black and update the info
                    |> fun (coloring, lookup, counter) ->
                        (coloring |> Map.add current Color.Black, (lookup.Update current <| fun info -> { info with finished = counter + 1 }), counter + 1)

                // fold over the vertices to build the lookup, starting from the source vertex (hence the initial value of accumulator)
                vertices
                |> Set.toList
                |> List.fold (fun (coloring: Map<_,_>, lookup, counter) vertex ->
                    match coloring.[vertex] with
                    | Color.White -> visit vertex counter (coloring, lookup)
                    | _ -> (coloring, lookup, counter)) (visit source 0 (initialColoring, ResultLookup(Map.empty, VertexLookupInfo.Empty)))
                // drop the coloring and the counter, we only care about lookup here
                |> fun (_, lookup, _) -> lookup.Unwrap

            | other -> failwithf "Not a graph: %A" other
