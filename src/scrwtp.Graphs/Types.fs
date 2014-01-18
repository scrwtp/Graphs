﻿namespace scrwtp.Graphs

open System

type Vertex = 
    | Label of string

type Edge = (Vertex * Vertex) * double
    
type Graph =
    | Directed of Set<Vertex> * Set<Edge>
    | Undirected of Set<Vertex> * Set<Edge>

type AdjacencyList = 
    | Simple of Map<Vertex, Set<Vertex>>
    | Weighted of Map<Vertex, Set<(Vertex * double)>>

type AdjacencyMatrix = 
    | Sparse of Map<Vertex * Vertex, double>

type ResultLookup<'TLookupInfo> (map : Map<Vertex, 'TLookupInfo>, empty : 'TLookupInfo) = 
    let update vertex f map =
        match map |> Map.tryFind vertex with
        | Some info -> map |> Map.add vertex (f info)
        | None      -> map |> Map.add vertex (f empty)

    member this.Unwrap = map

    member this.Find key = 
        map |> Map.find key

    member this.TryFind key = 
        map |> Map.tryFind key

    member this.FindOrEmpty key = 
        match this.TryFind key with
        | Some info -> info
        | None      -> empty        

    member this.Update vertex f =   
        ResultLookup(map |> update vertex f, empty)
      