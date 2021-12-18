open System.IO

// ------ DATA PREP ----------------
let rawData = File.ReadAllLines "day13.txt"

let separator =
    Seq.findIndex (String.length >> ((=) 0)) rawData

let toPosition (s: string) =
    s.Split ","
    |> (Seq.pairwise >> Seq.head)
    |> (fun (x, y) -> int y, int x)

type Instruction =
    | Y
    | X

let xORy =
    function
    | "fold along y" -> Y
    | _ -> X

let toInstruction (s: string) =
    s.Split "="
    |> (Seq.pairwise >> Seq.head >> fst >> xORy)

let to2dArray ds =
    let height, width =
        fst (Seq.maxBy fst ds) + 1, snd (Seq.maxBy snd ds) + 1

    let newArray = Array2D.create height width false
    Seq.iter (fun (x, y) -> newArray[ x, y ] <- true) ds
    newArray

let dots, instructions =
    rawData
    |> Seq.toList
    |> List.splitAt separator
    |> fun (l, r) ->
        to2dArray (Seq.map toPosition l), Seq.map toInstruction (Seq.tail r)

// ------ domain functions

let dimensions grid =
    Array2D.length1 grid, Array2D.length2 grid

let rotate grid =
    let height, width = dimensions grid
    Array2D.init width height (fun row column -> grid[height - column - 1, row])

let flip grid =
    let height, width = dimensions grid
    Array2D.init height width (fun row column -> grid[height - row - 1, column])

let midpoint grid = Array2D.length1 grid / 2

let combine (grid:bool[,]) =
    let top = grid[..(midpoint grid - 1), *]
    let bottom = flip (grid[(midpoint grid + 1).., *])
    let height, width = dimensions top
    Array2D.init height width (fun row column -> top[row, column] || bottom[row, column])

let folder grid = function
    | Y -> combine grid
    | X -> (rotate >> combine >> rotate >> rotate >> rotate) grid

// ----- part 1 ----
let day13part1solution () =
    Seq.fold folder dots (Seq.take 1 instructions)
    |> Seq.cast<bool>
    |> Seq.sumBy (fun b -> if b then 1 else 0)

//  answer: 607
 
 // --- part 2 ----

let toChar b = if b then "*" else "."

let day13part2solution () =
    let result =
        Seq.fold folder dots instructions
        |> Array2D.map toChar
        
    for i in 0..((Array2D.length1 result) - 1) do
        result[i, *]
        |> String.concat ""
        |> printfn "%A"

day13part2solution ()

// answer:
//".**..***..****.*....***..****.****.*...."
//"*..*.*..*....*.*....*..*.*.......*.*...."
//"*....*..*...*..*....*..*.***....*..*...."
//"*....***...*...*....***..*.....*...*...."
//"*..*.*....*....*....*....*....*....*...."
//".**..*....****.****.*....*....****.****."
