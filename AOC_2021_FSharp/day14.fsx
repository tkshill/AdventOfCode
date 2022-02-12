open System.IO

// --------------- data prep --------------------------
let rawData = File.ReadAllLines "day14.txt"
let initialValue = Seq.head rawData
let charsToString = Seq.map string >> String.concat ""
let leftmost, rightmost = Seq.head initialValue, Seq.last initialValue

let grouped =
    toPairs initialValue
    |> Seq.countBy id
    |> Seq.map (fun (s, n) -> s, int64 n)
    
let stringToMapping (s:string) = s.Split " -> " |> (fun s -> s[0], char s[1])
        
let changes = Seq.map stringToMapping rawData[2..]

// ------------ domain funcs ------------------

let toPairs = Seq.pairwise >> Seq.map (fun (l, r) -> charsToString [l; r])

let collectAndSum<'a when 'a: equality> : seq<'a * int64> -> seq<'a * int64>=
    Seq.groupBy fst >> Seq.map (snd >> (Seq.reduce (fun (f, s) (_, s2) -> f, (s + s2))))

let rec cycleChanges change item =
    let pair2, insert = Seq.head change
    let (pair:string), count = Seq.head item
    if pair = pair2 then
        let firstpair = [pair[0]; insert] |> charsToString
        let secondpair = [insert; pair[1]] |> charsToString 
        [(firstpair, count); (secondpair, count)]
    else
        cycleChanges (Seq.tail change) item

let updatePolymers =
    Seq.map List.singleton
    >> Seq.map (cycleChanges changes)
    >> Seq.concat
    >> collectAndSum<string>
    
let rec cyclePolymers cycles polymers =
    if cycles = 0 then polymers
    else
        let newPolymers = updatePolymers polymers
        cyclePolymers (cycles - 1) newPolymers

let solver n =
    cyclePolymers n grouped
    |> Seq.map (fun (s, c) -> [(s[0], c); (s[1], c)])
    |> Seq.concat
    |> Seq.append [(leftmost, 1); (rightmost, 1)]
    |> collectAndSum<char>
    |> Seq.map (fun (s, n) -> s, (n/2L))
    |> Seq.sortByDescending snd
    |> (fun p -> snd (Seq.head p), snd (Seq.last p))
    |> (fun p -> fst p - snd p)

let day14part1solution = solver 10
// 2988

let day14part2solution = solver 40
// 3572761917024L
