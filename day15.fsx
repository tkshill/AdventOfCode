open System
open System.IO

// HELPERS
let flip f a b = f b a

let uncurry f (a, b) = f a b

// DOMAIN

// the core data type for our application
type Node = { Position: int * int; Distance: int; Weight: int; Visited: bool }

// module of helper functions
[<AutoOpen>]
module Node =
    let visited n = n.Visited
    let distance n = n.Distance
    let position n = n.Position
    let weight n = n.Weight

// DATA PREP

let toNode (arr: string[]) h w =
    { Position = (h, w)
      Distance = Int32.MaxValue
      Weight = (int (arr[h][w])) - 48
      Visited = false }

let input =
    File.ReadAllLines "day15.txt"
    |> fun g -> Array2D.init g.Length g[0].Length (toNode g)
    
let size = input.GetLength(0)

// PART 1

let freshGrid graph =
    let newGrid = Array2D.copy graph
    newGrid[0, 0] <- { graph[0, 0] with Distance = 0 }
    newGrid
    
let tryFind (graph: Node[,]) (x, y) =
    try Some(graph[x, y])
    with | :? IndexOutOfRangeException -> None

let surroundings graph (h, w) =
    [ (0, 1); (1, 0); (0, -1); (-1, 0) ]
    |> Seq.map (fun (r, c) -> h + r, w + c)
    |> Seq.choose (tryFind graph)
        
let updateDistance current = function
    | node when node.Distance > current.Distance + node.Weight ->
        { node with Distance = current.Distance + node.Weight }
    | otherwise -> otherwise
        
let finalNode (graph: Node[,]) =
    let h, w = Array2D.length1 graph, Array2D.length2 graph
    graph[h - 1, w - 1]

let rec dijkstra updated (graph: Node[,]) current =
    let h, w = current.Position
    graph[h, w] <- { current with Visited = true }

    match current.Position with
    // stop when we visit the end node
    | p when p = position (finalNode graph) -> graph
    | p ->
        // get updated node values
        let updates =
            surroundings graph p
            |> Seq.filter (not << visited)
            |> Seq.map (updateDistance current)
        
        // update the graph positions with the new distances
        Seq.map (fun n -> n, n.Position) updates
        |> Seq.iter (fun (node, (h0, w0)) -> graph[ h0, w0 ] <- node)
        
        // update the set of positions that are no longer infinity away
        let newUpdates =
            Set.ofSeq updates
            |> Set.union updated
            |> Set.remove current
        
        // get the value with the minimum distance from the updated
        let next =
            Seq.minBy distance newUpdates
            |> position
            |> (uncurry (Array2D.get graph)) 
        
        dijkstra newUpdates graph next
        
let nextShortest graph = function
    | node when node.Position = (0, 0) -> None
    | node ->
        node.Position
        |> surroundings graph
        |> Seq.minBy distance
        |> fun nextNode -> Some(node, nextNode)
        
let shortestDistance graph =
    dijkstra Set.empty graph graph[0,0]
    |> fun ns -> Seq.unfold (nextShortest ns) (finalNode ns)
    |> Seq.sumBy weight
    
let day15part1solution () =
    shortestDistance (freshGrid input) 
    
// PART 2
let updateWeight h w n =
    let increased = n.Weight + h + w

    if increased > 9 then
        { n with Weight = increased - 9 }
    else
        { n with Weight = increased }

let expander graph h w _ =
    match h, w with
    | 0, 0 -> graph
    | _ -> Array2D.map (updateWeight h w) graph

let to2D (bigGraph: Node[,]) (graphs: Node[,][,]) =
    let updateNode bigR bigC r c node =
        let row = (bigR * size) + r
        let column = (bigC * size) + c
        bigGraph[ row, column ] <- { node with Position = row, column }

    Array2D.iteri (fun row col graph -> Array2D.iteri (updateNode row col) graph) graphs

    bigGraph

let day15part2solution () =
    let bigGraph = Array2D.zeroCreate (size * 5) (size * 5)

    Array2D.zeroCreate 5 5
    |> Array2D.mapi (expander input)
    |> to2D bigGraph
    |> freshGrid
    |> shortestDistance

printfn $"{day15part1solution ()}"    // 390 
printfn $"{day15part2solution ()}"    // 2814
