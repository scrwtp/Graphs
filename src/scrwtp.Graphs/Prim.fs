namespace scrwtp.Graphs

open System

(*
If a graph is empty then we are done immediately. Thus, we assume otherwise.

The algorithm starts with a tree consisting of a single vertex, and continuously increases its size one edge at a time, until it spans all vertices.

Input: A non-empty connected weighted graph with vertices V and edges E (the weights can be negative).
Initialize: Vnew = {x}, where x is an arbitrary node (starting point) from V, Enew = {}
Repeat until Vnew = V:
Choose an edge {u, v} with minimal weight such that u is in Vnew and v is not (if there are multiple edges with the same weight, any of them may be picked)
Add v to Vnew, and {u, v} to Enew
Output: Vnew and Enew describe a minimal spanning tree
*)

module Prim =
    let getMST graph = 
        match graph with
        | G (v, e) when Set.isEmpty v -> 
            G (Set.empty, Set.empty)
        | G (v, e) ->
            let rec getMST' graph tree = 
                match graph, tree with
                | G (v, _), G (v', _) as t when v = v' -> t
                | G (v, e), G (v', e') ->
                    // choose an edge
                    getMST' graph (G (v' |> Set.add newVertex, e' |> Set.add (Edge.Directed (u, newVertex, weight))))
                    

            getMST' graph (G (Set.ofSeq <| Seq.take 1 v, Set.empty))
    

