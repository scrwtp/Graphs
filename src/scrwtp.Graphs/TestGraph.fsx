#I "./bin/debug/"
#r "scrwtp.Graphs.dll"

open System
open scrwtp.Graphs

let graph = 
    let vertices = 
        [ "A"; "B"; "C"; "D"; "E"; "F" ]
        |> List.map Vertex.Label
        |> Set.ofList
    let edges = 
        [
            (Label "A", Label "B"), 2.0
            (Label "A", Label "F"), 1.0
            (Label "B", Label "C"), 3.0
            (Label "B", Label "E"), 1.0
            (Label "E", Label "D"), 4.0
        ]
        |> Set.ofList
    (vertices, edges)
    |> Graph.Directed

let adjList     = Graphs.buildAdjacencyList graph
let adjMatrix   = Graphs.buildAdjacencyMatrix graph

let pred1 = Search.DepthFirst.buildLookup graph (Label "A")
let pred2 = Search.DepthFirst.buildLookup graph (Label "D")

let pred3 = Search.BreadthFirst.buildLookup graph (Label "A")
let pred4 = Search.BreadthFirst.buildLookup graph (Label "D")