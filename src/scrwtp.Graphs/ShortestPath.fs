namespace scrwtp.Graphs

module ShortestPath =
    module Dijkstra =
        type VertexLookupInfo = 
            {
                predecessor: Vertex option
                distance: double
            }
            static member Empty = 
                {
                    predecessor = None
                    distance = 0.0
                }

        open Collections

        let buildLookup graph source = 
            match graph with
            | Graphs.Graph (vertices, edges) -> 
                // adjacency list for the graph
                let adjacencyList =
                    Graphs.buildAdjacencyList graph

                // function that gets the neighbouring nodes reachable from a vertex
                let neighbours =
                    Graphs.neighbours (Graphs.buildAdjacencyList graph)

                // set up the initial state of priority queue and result lookup
                let resultLookup, queue =
                    vertices 
                    |> Set.map (fun vertex ->
                        if vertex = source
                            then vertex, { predecessor = None; distance = 0.0 }
                            else vertex, { predecessor = None; distance = infinity })
                    |> fun infos ->
                        ResultLookup (infos |> Map.ofSeq, VertexLookupInfo.Empty), 
                        infos |> Set.fold (fun acc (vertex, info) -> acc |> Heap.insert (info.distance, vertex)) Heap.Empty 
                
                let rec visit queue (resultLookup : ResultLookup<_>) = 
                    // while queue is not empty...
                    if Heap.isEmpty queue
                        then resultLookup.Unwrap
                        else
                            // ...grab a vertex and its info
                            let current, queue = 
                                queue 
                                |> (Heap.findMin >> fun (min, heap) -> 
                                    min 
                                    // we have checked if the queue is empty before, so findMin has to return Some
                                    |> Option.get 
                                    |> fun (_, vertex) -> vertex, Heap.deleteMin heap)
                            
                            // get a set of edges going out from current vertex together with weights
                            let edges = 
                                match adjacencyList with 
                                | Weighted map -> map |> Map.find current |> Map.ofSeq
                                | Simple _ -> failwith "Simple adjacency list not supported"

                            // fold over its neighbours, updating results lookup
                            let updatedLookup, updatedQueue =
                                neighbours current
                                |> List.fold (fun (lookup : ResultLookup<_>, queue) vertex -> 
                                    let getDistance v = lookup.Find v |> fun info -> info.distance

                                    let distance        = getDistance current + (edges |> Map.find vertex)
                                    let currentDistance = getDistance vertex

                                    if distance < currentDistance
                                        then 
                                            let updatedQueue = 
                                                queue
                                                |> Heap.delete (currentDistance, vertex)
                                                |> Heap.insert (distance, vertex)

                                            let updatedLookup =
                                                lookup.Update vertex <| fun info -> 
                                                    { info with predecessor = Some current; distance = distance}

                                            (updatedLookup, updatedQueue)                                            
                                        else 
                                            (lookup, queue)) (resultLookup, queue)
                            
                            // try processing another vertex from the queue
                            visit updatedQueue updatedLookup

                visit queue resultLookup

            | other -> failwithf "Not a graph: %A" other

    module BellmanFord =
        ()

    module FloydWarshall =
        ()