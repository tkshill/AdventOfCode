open System.IO
open System.Collections.Generic

// ----------------- SETUP -----------------------

type Cave =
    | Start
    | End
    | Big of string
    | Small of string

type CaveMap = Dictionary<Cave, list<Cave>>
    
let toPair (str: string) = str.Split "-" |> Seq.pairwise |> Seq.head

let toCave =
    function
    | "start" -> Start
    | "end" -> End
    | s when s = s.ToUpper() -> Big s
    | s -> Small s

let rawData =
    File.ReadAllLines "day12.txt"
    |> Seq.map (toPair >> (fun (s1, s2) -> toCave s1, toCave s2))

let update (d:CaveMap) s1 s2 =
    if d.ContainsKey s1 then d[ s1 ] <- s2 :: d[ s1 ]
    else d.Add(s1, [ s2 ])
    
let d = CaveMap()
    
Seq.iter (fun (s1, s2) -> update d s1 s2; update d s2 s1) rawData

// --------------- PART ONE ----------------------------

let isValidProgression history = function
    | Start -> false
    | Small c when Seq.contains (Small c) history -> false
    | _ -> true
 
let rec recurser1 (d:CaveMap) history key =
     if key = End then 1
     else
         let nextSet = Seq.filter (isValidProgression history) d[key]
         if Seq.isEmpty nextSet then 0
         else
             let newHistory = key :: history
             Seq.sumBy (recurser1 d newHistory) nextSet
             
let day12part1solution = recurser1 d [] Start

// ---------------------------------- PART 2-----------------------------------


let small = function | Small _ -> true | _ -> false

let isValidProgression2 repeatAllowed history = function
    | Start -> false
    | Big _ -> true
    | End -> true
    | c when not (Seq.contains c history) -> true
    | _ when repeatAllowed -> true
    | _ -> false
    
let updateRepeatAllowed key history repeatAllowed =
    if small key && Seq.contains key history then false
    else repeatAllowed
    
let rec recurser2 (d:CaveMap) repeatAllowed history key =
    if key = End then 1
    else
        let newRp = updateRepeatAllowed key history repeatAllowed
        let nextSet = Seq.filter (isValidProgression2 newRp history) d[key]
        if Seq.isEmpty nextSet then 0
        else
            let newHistory = key :: history
            Seq.sumBy (recurser2 d newRp newHistory) nextSet
            
let day12part2solution = recurser2 d true [] Start
           
printfn $"{day12part1solution}, {day12part2solution}"
// 3485, 85062
