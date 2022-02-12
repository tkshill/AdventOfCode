namespace Aoc2021.day18
module ``Day 18`` =
    
    open System.IO
    open FParsec
    open FParsec.Pipes

    type SnailFishNumber = SnailFishValue * SnailFishValue
    and SnailFishValue = Lit of int | SFN of SnailFishNumber
    
    type Direction = Left | Right
    
    let floor' x y = (float x / float y) |> floor |> int
    let ceil' x y = (float x / float y) |> ceil |> int
    
    let pSFnumber, pSFnumberRef = createParserForwardedToRef()
    
    let pSFvalue = (pint32 |>> Lit) <|> (pSFnumber |>> SFN)
    
    do pSFnumberRef :=
        %% "[" -- +.pSFvalue -- "," -- +.pSFvalue -- "]"
        -|> fun a b -> a, b
    
    let toSFnumber str =
        match run pSFnumber str with
        | Success (sfNumber, _, _) ->
            sfNumber
            
        | _ ->
            failwith "goofed"
    
    let rec sfToString = function
        | Lit n2 ->
            string n2
            
        | SFN (v1, v2) ->
            $"[{sfToString v1},{sfToString v2}]"
        
    let rec spelunk direction value = function
        | Lit n ->
            Lit (n + value)
            
        | SFN (v1, v2) ->
            match direction with
            | Left ->
                SFN (spelunk direction value v1, v2)
                
            | Right ->
                SFN (v1, spelunk direction value v2)

    let rec explode depth = function
        | SFN (v1, v2) when depth < 4 ->
            let accrualLL, accrualRL, newLeft = explode (depth + 1) v1
            
            if newLeft <> v1 then
               accrualLL, 0, SFN (newLeft, spelunk Left accrualRL v2)
                
            else
                let accrualLR, accrualRR, newRight = explode (depth + 1) v2
                0, accrualRR, SFN ( spelunk Right accrualLR v1, newRight )
                
        | SFN (Lit n1, Lit n2) when depth = 4 ->
            n1, n2, Lit 0
        
        | otherwise ->
            0, 0, otherwise
            
    let rec split = function
        | Lit n when n >= 10 ->
            SFN ( Lit (floor' n 2), Lit (ceil' n 2) )
        
        | SFN (v1, v2) ->
            let v1' = split v1
            
            if v1' <> v1 then SFN (v1', v2)
            else SFN (v1, split v2)
                
        | otherwise -> otherwise
            
    let rec reduce sfValue =
        let _, _, sfValue2 = explode 0 sfValue
        if sfValue2 = sfValue then
            let sfValue3 = split sfValue2
            
            if sfValue3 = sfValue2 then
                sfValue3
            else
                reduce sfValue3
                
        else
            reduce sfValue2
            
    let rec magnitude = function
        | Lit n1, Lit n2 ->
            (3 * n1) + (2 * n2)
            
        | Lit n1, SFN sfNum ->
            (3 * n1) + (2 * magnitude sfNum)
            
        | SFN sfNum, Lit n2 ->
            (3 * magnitude sfNum) + (2 * n2)
            
        | SFN leftNum, SFN rightNum ->
            (3 * magnitude leftNum) + (2 * magnitude rightNum)
        
    let day18part1solution =
        File.ReadAllLines
        >> Seq.map (toSFnumber >> SFN)
        >> Seq.reduce (fun a b -> SFN (a, b) |> reduce)
        >> function | SFN num -> magnitude num | _ -> failwith "not a number"
        
    // Part 2
    
    // Thanks to Thomas Patriczek for this implementation of combinations
    let rec combinations acc size set = seq {
        match size, set with
        | n, x::xs ->
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs
        | 0, [] -> yield acc
        | _, [] -> () 
    } 
    
    let uniquePairs =
        let pairs n = [(Seq.head n, Seq.last n); (Seq.last n, Seq.head n)]
        combinations [] 2 >> Seq.collect pairs
        
    let day18part2solution =
        File.ReadAllLines
        >> Seq.map (toSFnumber >> SFN)
        >> Seq.toList
        >> uniquePairs
        >> Seq.map (SFN >> reduce >> function | SFN num -> magnitude num | _ -> failwith "not a number")
        >> Seq.max
    
module ``Day 18 Tests`` =
    
    open FsUnit.Xunit
    open Xunit
    open ``Day 18``
    
    type ``Part 1 tests`` () =
        
        [<Fact>]
        member x.``strings can successfully be converted to SnailFish Numbers`` () =
            toSFnumber "[1,2]"
                |> should equal ( Lit 1, Lit 2 )
            toSFnumber "[9,[8,7]]"
                |> should equal ( Lit 9, SFN ( Lit 8, Lit 7 ) )
            toSFnumber "[[1,9],[8,5]]"
                |> should equal ( SFN ( Lit 1, Lit 9 ), SFN ( Lit 8, Lit 5 ) )
        
        [<Fact>]
        member x.``toString works correctly`` () =
            sfToString <| SFN ( Lit 1, Lit 2 )
                |> should equal "[1,2]"
            sfToString <| SFN ( Lit 9, SFN ( Lit 8, Lit 7 ) )
                |> should equal "[9,[8,7]]"
            sfToString <| SFN ( SFN ( Lit 1, Lit 9 ), SFN ( Lit 8, Lit 5 ) )
                |> should equal "[[1,9],[8,5]]"
