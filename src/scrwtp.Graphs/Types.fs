namespace scrwtp.Graphs

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
