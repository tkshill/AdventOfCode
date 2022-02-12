open System.IO

let rawdata = File.ReadAllLines "day9.txt"

let width, height =
    Seq.length (Seq.head rawdata), Seq.length rawdata

let data =
    rawdata
    |> Seq.concat
    |> Seq.map (string >> int)
    |> Seq.indexed

let adjacents w h n =
    let top, bottom, left, right =
        (if n - w >= 0 then Some(n - w) else None),
        (if n + w < w * h then
             Some(n + w)
         else
             None),
        (if n % w <> 0 then Some(n - 1) else None),
        (if (n + 1) % w <> 0 then
             Some(n + 1)
         else
             None)

    Seq.choose id [ top; right; bottom; left ]

let lowPointValue (i, n) =
    match n with
    | 9 -> None
    | _ ->
        adjacents width height i
        |> Seq.map ((fun j -> Seq.find (fst >> ((=) j)) data) >> snd)
        |> Seq.forall ((<) n)
        |> fun b -> if b then Some(n + 1) else None

let day9part1solution =
    data
    |> Seq.map lowPointValue
    |> Seq.choose id
    |> Seq.sum

let intersects surrounding basin =
    Set.count (Set.intersect (Set.map fst basin) surrounding) > 0

let updateBasins state (idx, num) =
    if num = 9 then
        state
    else
        let surrounding = adjacents width height idx |> Set.ofSeq

        if not <| Seq.exists (intersects surrounding) state then
            (Set.singleton (idx, num)) :: state
        else
            List.map
                (fun basin ->
                    if (intersects surrounding basin) then
                        Set.add (idx, num) basin
                    else
                        basin)
                state

let surroundings basin =
    basin
    |> Set.map (fst >> (adjacents width height) >> Set.ofSeq)
    |> Set.unionMany
    
    

let collapseBasins newBasins (basin: Set<int * int>) =
    if not (Seq.exists (intersects (surroundings basin)) newBasins) then
        basin :: newBasins
    else
        List.map
            (fun newBasin ->
                if intersects (surroundings basin) newBasin then
                    Set.union newBasin basin
                else
                    newBasin)
            newBasins

let day9part2solution =
    data
    |> Seq.fold updateBasins List.empty
    |> Seq.fold collapseBasins List.empty
    |> Seq.sortByDescending (Set.count)
    |> Seq.take 3
    |> Seq.map Set.count
    |> Seq.reduce (*)

printfn "%A" (day9part1solution, day9part2solution)
