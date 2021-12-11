open System.IO

let flip f a b = f b a

type Token =
    | Empty
    | Incomplete of char * Token
    | Valid of char * char
    | Invalid of char * char

let toNum =
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

let isOpener = (=) >> flip Seq.exists [ '('; '['; '{'; '<' ]

let isInvalid =
    function
    | Invalid _ -> true
    | _ -> false

let pairValue =
    function
    | Invalid (_, c) -> toNum c

let data = File.ReadAllLines "day10test.txt"

let tokenize (head :: tail) c =
    match head with
    | Empty -> Incomplete(c, Empty) :: tail
    | Incomplete _ when isOpener c -> Incomplete(c, head) :: tail
    | Incomplete (o, pair) when opposite o = c -> pair :: Valid(o, c) :: tail
    | Incomplete (o, pair) -> pair :: Invalid(o, c) :: tail

let day10part1solution =
    data
    |> Seq.map (Seq.fold tokenize (List.singleton Empty))
    |> Seq.choose (Seq.tryFindBack isInvalid)
    |> Seq.sumBy pairValue

(*----------------------------------------------------------------*)

let deTokenize =
    function
    | Empty -> None
    | Incomplete (c, pair) -> Some (opposite c, pair)

let isIncomplete =
    function
    | Incomplete _ -> true
    | _ -> false

let toNum2 =
    function
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L

let score s c = s * 5L + c

let middle = Seq.sort >> Seq.splitInto 2 >> Seq.head >> Seq.last

let day10part2solution =
    data
    |> Seq.map (Seq.fold tokenize (List.singleton Empty))
    |> Seq.filter (Seq.forall (not << isInvalid))
    |> Seq.choose (Seq.tryFindBack isIncomplete)
    |> Seq.map ((Seq.unfold deTokenize >> Seq.map toNum2) >> Seq.fold score 0L)
    |> middle

printfn $"{day10part1solution}, {day10part2solution}"

// (294195, 3490802734L)
