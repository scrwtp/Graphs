namespace scrwtp.Graphs

module Collections = 
    /// Leftist heap (from Okasaki)
    type Heap<'T> =
        | Empty
        | Node of int * 'T * Heap<'T> * Heap<'T>

    module Heap =
        let empty<'T> () =
            Heap.Empty : Heap<'T>

        let isEmpty = function
            | Empty -> true
            | _     -> false             

        let rank = function
            | Empty -> 0
            | Node (rank, _, _, _) -> rank            

        let makeNode t a b =
            let ra, rb = rank a, rank b
            if ra >= rb 
                then Node (rb + 1, t, a, b)
                else Node (ra + 1, t, b, a)

        let rec merge<'T when 'T : comparison> (heap1:Heap<'T>) (heap2:Heap<'T>) =
            match heap1, heap2 with
            | Empty, h
            | h, Empty -> h
            | (Node (_, t1, a1, b1) as h1), (Node (_, t2, a2, b2) as h2) ->
                if t1 <= t2
                    then makeNode t1 a1 (merge b1 h2)
                    else makeNode t2 a2 (merge h1 b2)

        let insert t heap =
            let single = Node (1, t, Empty, Empty)
            merge single heap

        let findMin = function
            | Empty -> failwith "The heap is empty."
            | Node (_, t, _, _) -> Some t

        let deleteMin = function
            | Empty -> failwith "Can't delete from an empty heap."
            | Node (_, t, a, b) -> merge a b

        let fromSeq coll =
            let singletons =
                coll 
                |> Seq.map(fun elem -> Node (1, elem, Empty, Empty))
                |> List.ofSeq
            let rec inner heaps acc = 
                match heaps, acc with
                | [],   []  -> Empty
                | [],   [a] -> a
                | [],   acc -> inner acc [] 
                | [s],  acc     -> inner [] (s::acc)
                | a::b::t, acc  -> inner t ((merge a b)::acc)
            inner singletons []                

        let toSeq heap =
            let rec inner heap acc = 
                match heap with
                | Empty -> acc |> List.rev |> Seq.ofList
                | h     -> inner (deleteMin h) ((findMin h)::acc)
            inner heap []