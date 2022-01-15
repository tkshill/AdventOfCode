namespace Aoc2021.day18
module ``Day 18`` =
    
    open System.IO
    open FParsec
    open FParsec.Pipes

    type SnailFishNumber = SnailFishValue * SnailFishValue
    and SnailFishValue = Lit of int | SFN of SnailFishNumber
    
    type Direction = Left | Right
    
    let floor_ x y = (float x / float y) |> floor |> int
    let ceil_ x y = (float x / float y) |> ceil |> int
    
    let pSFnumber, pSFnumberRef = createParserForwardedToRef()
    
    let pSFvalue = (pint32 |>> Lit) <|> (pSFnumber |>> SFN)
    
    do pSFnumberRef :=
        %% "[" -- +.pSFvalue -- "," -- +.pSFvalue -- "]" -|> fun a b -> a, b
    
    let toSFnumber str =
        match run pSFnumber str with
        | Success (sfNumber, _, _) -> sfNumber
        | _ -> failwith "goofed"
    
    let rec sfToString = function
        | Lit n, Lit n2 -> $"[{n},{n2}]"
        | SFN n, SFN n2 -> $"[{sfToString n},{sfToString n2}]"
        | Lit n, SFN n2 -> $"[{n},{sfToString n2}]"
        | SFN n, Lit n2 -> $"[{sfToString n},{n2}]"

    let combine n1 n2 = SFN n1, SFN n2
        
    let rec spelunk direction value = function
        | Lit n -> Lit (n + value)
        | SFN n ->
            match direction with
            | Left -> SFN (spelunk direction value (fst n), snd n)
            | Right -> SFN (fst n, spelunk direction value (snd n))
    
    let rec explode depth = function 
        | SFN sfNumberL, SFN sfNumberR when depth < 4 ->
            let accrualLL, newLeft, accrualRL = explode (depth + 1) sfNumberL
            
            if newLeft <> sfNumberL then
                accrualLL, ( SFN newLeft, spelunk Left accrualRL (SFN sfNumberR) ), 0
            else
                let accrualLR, newRight, accrualRR = explode (depth + 1) sfNumberR
                0, ( spelunk Right accrualLR (SFN sfNumberL), SFN newRight ), accrualRR
                
        | SFN sfNumberL, sfValueR when depth < 4 ->
            let accrualL, newLeft, accrualR = explode (depth + 1) sfNumberL
            
            accrualL, ( SFN newLeft, spelunk Left accrualR sfValueR ), 0
            
        | sfValueL, SFN sfNumberR when depth < 4 ->
            let l', newRight, r' = explode (depth + 1) sfNumberR
            
            0, ( spelunk Right l' sfValueL, SFN newRight ), r'
            
        | SFN ( Lit numL, Lit numR ), sfValueR when depth = 4 ->
            numL, ( Lit 0, spelunk Left numR sfValueR ), 0
            
        | sfValueL , SFN (Lit numL, Lit numR) when depth = 4 ->
            0, ( spelunk Right numL sfValueL, Lit 0 ), numR
        
        | otherwise -> 0, otherwise, 0
            
    let rec split = function
        | Lit numX, rightValue when numX >= 10 ->
            SFN ( Lit (floor_ numX 2), Lit (ceil_ numX 2) ), rightValue
            
        | Lit numX, Lit numY when numY >= 10 ->
            Lit numX, SFN ( Lit (floor_ numY 2), Lit (ceil_ numY 2) )
            
        | Lit numX, SFN rightNumber ->
            Lit numX, SFN (split rightNumber)
            
        | SFN leftSFnumber, rightValue ->
            let newLeft = split leftSFnumber
            
            if newLeft <> leftSFnumber then
                SFN newLeft, rightValue
                
            else
                match rightValue with
                | Lit numY when numY >= 10 ->
                    SFN leftSFnumber, SFN (Lit (floor_ numY 2), Lit (ceil_ numY 2) )
                    
                | SFN rightNumber ->
                    SFN leftSFnumber, SFN (split rightNumber)
                    
                | _ ->
                    SFN leftSFnumber, rightValue
                    
        | otherwise -> otherwise
                    
    let rec reduce sfNumber0 =
        let _, sfNumber1, _ = explode 1 sfNumber0
        
        if sfNumber1 = sfNumber0 then
            let sfNumber2 = split sfNumber1
            
            if sfNumber2 = sfNumber1 then sfNumber2
            else reduce sfNumber2
        else
            reduce sfNumber1
            
    let rec magnitude = function
        | Lit n1, Lit n2 -> (3 * n1) + (2 * n2)
        | Lit n1, SFN rightValue -> (3 * n1) + (2 * magnitude rightValue)
        | SFN leftValue, Lit n2 -> (3 * magnitude leftValue) + (2 * n2)
        | SFN leftValue, SFN rightValue -> (3 * magnitude leftValue) + (2 * magnitude rightValue)
        
    let day18part1solution =
        File.ReadAllLines
        >> Seq.map toSFnumber
        >> Seq.reduce (fun n1 n2 -> combine n1 n2 |> reduce)
        >> magnitude
        
    // part 2
    
    let rec combinations acc size set = seq {
        match size, set with
        | n, x::xs ->
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs
        | 0, [] -> yield acc
        | _, [] -> () 
    } 
    
    let uniquePairs =
        combinations [] 2
        >> Seq.collect (fun n -> [(Seq.head n, Seq.last n); (Seq.last n, Seq.head n)])
        
    let day18part2solution =
        File.ReadAllLines
        >> Seq.map toSFnumber
        >> Seq.toList
        >> uniquePairs
        >> Seq.map (fun (n1, n2) -> combine n1 n2 |> reduce |> magnitude)
        >> Seq.max
        
    
module ``Day 18 Tests`` =
    
    open FsUnit.Xunit
    open Xunit
    open ``Day 18``
    
    type ``Part 1 tests`` () =
             
        let rec explode_ (sfNumber0: SnailFishNumber) =
          let _, sfNumber1, _ = explode 1 sfNumber0
        
          if sfNumber1 = sfNumber0 then sfNumber1
          else explode_ sfNumber1
        
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
            sfToString ( Lit 1, Lit 2 )
                |> should equal "[1,2]"
            sfToString ( Lit 9, SFN ( Lit 8, Lit 7 ) )
                |> should equal "[9,[8,7]]"
            sfToString ( SFN ( Lit 1, Lit 9 ), SFN ( Lit 8, Lit 5 ) )
                |> should equal "[[1,9],[8,5]]"
                
        [<Fact>]
        member x.``Snail Fish Numbers can be successfully combined`` () =
            combine (toSFnumber "[1,2]") (toSFnumber "[[3,4],5]")
                |> should equal (toSFnumber "[[1,2],[[3,4],5]]")
            combine (toSFnumber "[9,[8,7]]") (toSFnumber "[[1,9],[8,5]]")
                |> should equal (toSFnumber "[[9,[8,7]],[[1,9],[8,5]]]")
            
        [<Theory>]
        [<InlineData("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")>]
//        [<InlineData("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")>]
//        [<InlineData("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")>]
//        [<InlineData("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
        member x.``EXPLOSIONS test`` (input, expected) =
            input (toSFnumber >> explode_) |> should equal (toSFnumber expected)
 
