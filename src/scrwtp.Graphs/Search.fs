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

                // helper function for updating results lookup
                let update map vertex f = 
                    match map |> Map.tryFind vertex with
                    | Some info -> map |> Map.add vertex (f info)
                    | None      -> map |> Map.add vertex (f VertexLookupInfo.Empty)

                // mutable queue for storing vertices
                let queue = System.Collections.Generic.Queue<Vertex>()
                
                let rec visit (queue: System.Collections.Generic.Queue<_>) vertexColoring resultLookup = 
                    // while queue is not empty...
                    if queue.Count = 0
                        then resultLookup
                        else
                            // ...grab a vertex and its info
                            let current = queue.Dequeue()
                            let currentInfo = 
                                match resultLookup |> Map.tryFind current with
                                | Some info -> info
                                | None -> VertexLookupInfo.Empty
                            
                            // fold over its neighbours, updating results lookup
                            let updatedColored, updatedLookup =
                                neighbours current
                                |> List.fold (fun (coloring : Map<Vertex, _>, lookup) vertex -> 
                                    match coloring.[vertex] with
                                    | Color.White ->
                                        // we haven't seen the vertex, put it in the queue and update its info
                                        queue.Enqueue vertex
                                        let updatedLookup =
                                            update lookup vertex <| fun info -> 
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
                visit queue initialColoring Map.empty

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

                // helper function for updating results lookup
                let update map vertex f = 
                    match map |> Map.tryFind vertex with
                    | Some info -> map |> Map.add vertex (f info)
                    | None      -> map |> Map.add vertex (f VertexLookupInfo.Empty)

                let rec visit current counter (vertexColoring:Map<_,_>, resultLookup) = 
                    // it's a new vertex; tag it as discovered and mark it gray
                    let initial = 
                        (vertexColoring |> Map.add current Color.Gray,
                         (update resultLookup current <| fun info -> { info with discovered = counter + 1 }),
                         counter)

                    // fold over its neighbours, updating results lookup
                    neighbours current 
                    |> List.fold (fun (coloring:Map<_,_>, lookup, counter) vertex ->
                        match coloring.[vertex] with
                        | Color.White -> 
                            // we haven't seen the vertex; visit it recursively
                            visit vertex counter (coloring, update lookup vertex <| fun info -> { info with predecessor = Some current })
                        | _ -> (coloring, lookup, counter)) initial
                    // mark the current vertex black and update the info
                    |> fun (coloring, lookup, counter) ->
                        (coloring |> Map.add current Color.Black, (update lookup current <| fun info -> { info with finished = counter + 1 }), counter + 1)

                // fold over the vertices to build the lookup, starting from the source vertex (hence the initial value of accumulator)
                vertices
                |> Set.toList
                |> List.fold (fun (coloring: Map<_,_>, lookup, counter) vertex ->
                    match coloring.[vertex] with
                    | Color.White -> visit vertex counter (coloring, lookup)
                    | _ -> (coloring, lookup, counter)) (visit source 0 (initialColoring, Map.empty))
                // drop the coloring and the counter, we only care about lookup here
                |> fun (_, lookup, _) -> lookup

            | other -> failwithf "Not a graph: %A" other
