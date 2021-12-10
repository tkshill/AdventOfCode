open System.IO
open System

let flip f a b = f b a

type Pair =
    | Empty
    | Incomplete of char * Pair
    | ValidComplete of char * char
    | InvalidComplete of char * char

let toScore =
    function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

let opposite =
    function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'

let beginnings = [ '('; '['; '{'; '<' ]

let isOpeners = (=) >> flip Seq.exists beginnings

let isInvalid =
    function
    | InvalidComplete _ -> true
    | _ -> false

let pairValue =
    function
    | InvalidComplete (o, c) -> toScore c

let data = File.ReadAllLines "day10.txt"

let folder (head :: tail) c =
    match head with
    | Empty -> Incomplete(c, Empty) :: tail
    | Incomplete _ when isOpeners c -> Incomplete(c, head) :: tail
    | Incomplete (o, pair) when opposite o = c -> pair :: ValidComplete(o, c) :: tail
    | Incomplete (o, pair) -> pair :: InvalidComplete(o, c) :: tail

let day10part1solution =
    data
    |> Seq.map (Seq.fold folder (List.singleton Empty))
    |> Seq.choose (Seq.tryFindBack (isInvalid))
    |> Seq.sumBy pairValue

(*----------------------------------------------------------------*)

let rec folder2 state =
    function
    | Empty -> Seq.rev state
    | Incomplete (c, pair) -> folder2 (opposite c :: state) pair

let isIncomplete =
    function
    | Incomplete _ -> true
    | _ -> false

let toScore2 =
    function
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L

let day10part2solution =
    data
    |> Seq.map (Seq.fold folder (List.singleton Empty))
    |> Seq.filter (Seq.forall (not << isInvalid))
    |> Seq.choose (Seq.tryFindBack (isIncomplete))
    |> Seq.map (
        ((folder2 List.Empty) >> (Seq.map toScore2))
        >> (Seq.fold (fun score c -> score * 5L + c) 0L)
    )
    |> Seq.sort
    |> Seq.splitInto 2
    |> Seq.head
    |> Seq.last

printfn "%A" (day10part1solution, day10part2solution)

// (294195, 3490802734L)
