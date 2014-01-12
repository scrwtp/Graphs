#I "./bin/debug/"
#r "scrwtp.Graphs.dll"

open System
open scrwtp.Graphs
open scrwtp.Graphs.Collections

let test name a b =
    if a = b 
        then printfn "%s passed." name
        else printfn "%s FAILED!" name

let input = [ 1; 23; 54; 3; 12; 17; 3; 5; 6; 9; 2]

let heap1 =
    input
    |> Heap.fromSeq

test
    "seq -> heap -> seq results in sorted seq"
    (heap1 |> Heap.toSeq |> List.ofSeq)
    (input |> List.sort)