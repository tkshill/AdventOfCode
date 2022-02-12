open System.IO

// ---- rando helper functions
let flip f a b = f b a
let rev a f = f a

// -- Our core data type
type Number =
    | Regular of int
    | Flashing 
    | Flashed
    
// Number functions

let toInt = function
    | Flashing -> 1
    | _ -> 0
    
let flash = function
    | Regular n when n > 9 -> Flashing
    | otherwise -> otherwise
    
let dim = function
    | Flashing -> Flashed
    | otherwise -> otherwise
    
let increment m = function
    | Regular n -> Regular (n + m)
    | otherwise -> otherwise
    
let reset = function
    | Flashed -> Regular 0
    | otherwise -> otherwise
    
let isPrimed = function
    | Regular n when n > 9 -> true
    | _ -> false
    
let isFlashing = function
    | Flashing -> true
    | _ -> false
    
// Not all ticks are ticks
    
type Increment = Major | Minor with
    member this.Num = if this = Major then 1 else 0
    member this.Update = if this = Major then reset >> increment 1 >> flash else flash 
        

type Tick = int * Increment

// data prep   
let rawData = File.ReadAllLines "day11.txt"

let width, height =
    Seq.length (Seq.head rawData), Array.length rawData

let stream = rawData |> Seq.collect (Seq.map (string >> int >> Regular)) |> Seq.toArray

let adjacents n =
    let isN, tooLow, tooHigh, notLeft, notRight =
        ((=) n),
        (flip (<) 0),
        (flip (>=) (width * height)),
        ((=)
         >> flip
             Seq.exists
             [ (n - 1)
               (n + width - 1)
               (n - width - 1) ]
         >> (&&) (n % width = 0)),
        ((=)
         >> flip
             Seq.exists
             [ (n + 1)
               (n + width + 1)
               (n - width + 1) ]
         >> (&&) ((n + 1) % width = 0))

    Seq.filter
        (rev
         >> flip
             Seq.map
             [ isN
               tooLow
               tooHigh
               notLeft
               notRight ]
         >> Seq.forall not)
        (seq {
            for h in -1 .. 1 do
                for v in -1 .. 1 -> n + (width * h) + v
         })

let nextTick data =
    if Seq.isEmpty (Seq.filter isPrimed data) then Major else Minor
    
// solve part 1 with unfolds
let unfolder (data, (tick, inc:Increment)) =
    if tick >= 100 then None
    else 
        let updated = Array.map inc.Update data
        let flashes = Seq.sumBy toInt updated
        let affected =
            updated
            |> Seq.indexed
            |> Seq.filter (snd >> isFlashing)
            |> Seq.collect (fst >> adjacents)
            |> Seq.countBy id 
        let furtherUpdates = Array.map dim updated
        Seq.iter (fun (i, v) -> furtherUpdates[i] <- increment v furtherUpdates[i]) affected
        let incr = nextTick furtherUpdates
        
        Some (flashes, (furtherUpdates, (tick + incr.Num, incr)))
        
let day11part1solution = Seq.unfold unfolder (stream, Tick (0, Major)) |> Seq.sum

// -------------------------------------------------------------------

// solve part two with regular recursion
let toInt2 = function
    | Flashing
    | Flashed -> 1
    | _ -> 0
        
let rec recurser flashes data tick (inc:Increment) =
    if flashes = 100 then tick
    else
        let update1 = Array.map inc.Update data
        let flashes = Seq.sumBy toInt2 update1
        let affected =
            update1
            |> Seq.indexed
            |> Seq.filter (snd >> isFlashing)
            |> Seq.collect (fst >> adjacents)
            |> Seq.countBy id
        let update2 = Array.map dim update1
        Seq.iter (fun (i, v) -> update2[i] <- increment v update2[i]) affected
        let incr = if Seq.isEmpty (Seq.filter isPrimed update2) then Major else Minor
        
        recurser flashes update2 (tick + incr.Num) incr
        
let day11part2solution = recurser 0 stream 0 Major

printfn $"{day11part1solution} {day11part2solution}"
