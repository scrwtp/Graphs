namespace scrwtp.Graphs

module Collections = 
    /// Leftist heap (from Okasaki).
    /// Supports lazy deletion, makes it easier to implement Dijkstra's algorithm this way.
    type Heap<'T> =
        | Empty
        | Node of int (*rank*) * bool (*deleted*) * 'T (*element*) * Heap<'T> (*left child*) * Heap<'T> (*right child*)

    /// Operations on a leftist heap.
    module Heap =
        /// Returns an empty heap of type 'T.
        let empty<'T> () =
            Heap.Empty : Heap<'T>

        /// Gets the rank of a node.
        let rank = function
            | Empty -> 0
            | Node (rank, _, _, _, _) -> rank            

        /// Creates a node with element t and children a and b.
        let makeNode t a b =
            let ra, rb = rank a, rank b
            if ra >= rb 
                then Node (rb + 1, false, t, a, b)
                else Node (ra + 1, false, t, b, a)

        /// Merges two heaps together.
        let rec merge (heap1:Heap<'T>) (heap2:Heap<'T>) =
            match heap1, heap2 with
            | Empty, h
            | h, Empty -> h
            | (Node (_, _, t1, a1, b1) as h1), (Node (_, _, t2, a2, b2) as h2) ->
                if t1 <= t2
                    then makeNode t1 a1 (merge b1 h2)
                    else makeNode t2 a2 (merge h1 b2)

        /// Inserts element t into the heap.
        let insert t heap =
            let single = Node (1, false, t, Empty, Empty)
            merge single heap

        /// Generic fold over a heap.
        let fold func emptyState heap =
            let rec inner heap cont = 
                match heap with  
                | Empty -> cont emptyState
                | Node (rank, deleted, t, a, b) ->
                    inner a (fun aacc -> 
                        inner b (fun bacc ->
                            cont (func rank deleted t aacc bacc)))
            inner heap id

        /// Finds the minimum element, None for empty heap.
        /// Deletes marked elements from the heap.
        let findMin heap = 
            fold (fun rank deleted t (amin, a) (bmin, b) ->
                let heap =
                    if deleted
                        then (merge a b)
                        else Node (rank, deleted, t, a, b)
                let min =
                    [ amin; bmin; (if deleted then None else Some t)]
                    |> List.choose id
                    |> fun coll ->
                        if List.isEmpty coll
                            then None
                            else Some <| List.min coll
                                        
                (min, heap)) (None, Empty) heap

        // Checks if the heap is empty; implemented in terms of fold due to lazy deletion
        let isEmpty heap =
            fold (fun _ deleted _ isLeftEmpty isRightEmpty -> deleted && isLeftEmpty && isRightEmpty) true heap

        /// Marks first occurence of an element td for deletion.
        /// Since the heap implements lazy deletion, the element is only marked for deletion.
        let rec delete td heap =
            (fold (fun rank deleted t a b (acc, marked) ->
                if not marked && t = td
                    then Node (rank, true, t, a (acc, true), b (acc, true))
                    else Node (rank, deleted, t, a (acc, marked), b (acc, marked))) (fun (acc, marked) -> acc) heap) (Empty, false)

        /// Removes the minimum element from the heap.
        /// Deletes marked elements from the heap.
        let deleteMin heap =
            findMin heap
            |> fun (min, heap) ->
                match min with
                | Some m -> delete m heap
                | None   -> heap

        /// Converts a sequence into a leftist heap.
        let fromSeq coll =
            let singletons =
                coll 
                |> Seq.map(fun elem -> Node (1, false, elem, Empty, Empty))
                |> List.ofSeq
            let rec inner heaps acc = 
                match heaps, acc with
                | [],   []  -> Empty
                | [],   [a] -> a
                | [],   acc -> inner acc [] 
                | [s],  acc     -> inner [] (s::acc)
                | a::b::t, acc  -> inner t ((merge a b)::acc)
            inner singletons []                

        /// Converts a heap into a sorted sequence.
        let toSeq heap =
            let rec inner heap acc = 
                let min, _ = findMin heap
                match min with
                | None -> List.rev acc
                | Some m -> inner (deleteMin heap) (m::acc)
            inner heap []
            |> Seq.ofList