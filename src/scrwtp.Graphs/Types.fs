namespace scrwtp.Graphs

open System

type Vertex = 
    | Label of string

type Edge =
    | Directed of Vertex * Vertex * double
    | Undirected of Vertex * Vertex * double

type Graph =
    | G of Set<Vertex> * Set<Edge>

type AdjacencyList = 
    | Simple of Map<Vertex, Vertex list>
    | Weighted of Map<Vertex, (Vertex * double) list>

type AdjacencyMatrix = 
    | Sparse of Set<Vertex * Vertex> * (double * double * double)
    | Array of Vertex[] * double[,]
