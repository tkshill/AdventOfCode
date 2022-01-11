namespace Aoc2021.day17
module Day17 =
    
    type Target = { MinX: int; MaxX: int; MinY: int; MaxY: int }

    let quad a b c =
        let D = b * b - 4.*a * c
        [(+);(-)] |> List.map (fun f -> (f -b (sqrt D))/2./a) |> Seq.head |> ceil |> int
 
    let initialVelocities { MinX = x0; MaxX = x; MinY = y0; } =
        let minX = quad 1 1 ((float -2) * (float x0))
        seq { for i in minX .. x do for j in y0 .. abs y0 -> (i, j) }
    
    let tick (vx, vy) (x, y) = ((max (vx - 1) 0), vy - 1), (x + vx, y + vy)
    
    let isPast { MaxX = x; MinY = y0; } = function
        | i, j when i > x || j < y0 -> true
        | _ -> false
        
    let inTarget { MinX = x0; MaxX = x; MinY = y0; MaxY = y } = function
        | i, j when i >= x0 && i <= x && j >= y0 && j <= y -> true
        | _ -> false
    
    let advancePath target (velocity, position) =
        if isPast target position then None
        else Some (position, tick velocity position)
        
    let day17part1solution target =
        let addInit v = v,  (0, 0)
        target
        |> initialVelocities
        |> Seq.map (addInit >> Seq.unfold (advancePath target))
        |> Seq.filter (Seq.last >> inTarget target)
        |> Seq.map (Seq.map snd >> Seq.max)
        |> Seq.max
    
    let day17part2solution target =
        let addInit v = v,  (0, 0) 
        target
        |> initialVelocities
        |> Seq.map (addInit >> Seq.unfold (advancePath target))
        |> Seq.filter (Seq.last >> inTarget target)
        |> Seq.length
    
   
module ``Day 17 Tests`` =
    
    open FsUnit.Xunit
    open Xunit
    open Day17
    
    type ``Part 1 tests`` () =
        let input = "target area: x=119..176, y=-141..-84"
        let testTarget = { MinX = 3; MaxX = 5; MinY = -5; MaxY = -3 }
        let testTarget2 = { MinX = 20; MaxX = 30; MinY = -10; MaxY = -5 }       
        let trueTarget = { MinX = 119; MaxX = 176; MinY = -141; MaxY = -84 }
        
        
        [<Fact>]
        member x.``A target produces a valid sequence of initial velocities`` () =
            testTarget
            |> initialVelocities |> Seq.toList |> should equal
                [(2, -5); (2, -4); (2, -3); (2, -2); (2, -1); (2, 0); (2, 1); (2, 2); (2, 3); (2, 4); (2, 5)
                 (3, -5); (3, -4); (3, -3); (3, -2); (3, -1); (3, 0); (3, 1); (3, 2); (3, 3); (3, 4); (3, 5)
                 (4, -5); (4, -4); (4, -3); (4, -2); (4, -1); (4, 0); (4, 1); (4, 2); (4, 3); (4, 4); (4, 5)
                 (5, -5); (5, -4); (5, -3); (5, -2); (5, -1); (5, 0); (5, 1); (5, 2); (5, 3); (5, 4); (5, 5)]
            
        [<Fact>]
        member x.`` tick moves velocities and positions correctly`` () =
            tick (3, 3) (0, 0) |> should equal ((2, 2), (3, 3))
            tick (2, 0) (5, 5) |> should equal ((1, -1), (7, 5))
            tick (0, 2) (5, 5) |> should equal ((0, 1), (5, 7))
            tick (3, -3) (3, -3) |> should equal ((2, -4), (6, -6))
            
        [<Fact>]
        member x.``isInTarget correctly identifies positions`` () =
            inTarget testTarget (4, -4) |> should be True
            inTarget testTarget (3, -3) |> should be True
            inTarget testTarget (6, -4) |> should be False
            inTarget testTarget (4, -2) |> should be False
            
        [<Fact>]
        member x.``isPast correctly identifies positions after the target`` () =
            isPast testTarget (4, -4) |> should be False
            isPast testTarget (3, -6) |> should be True
            isPast testTarget (6, -4) |> should be True
            isPast testTarget (4, -2) |> should be False
            
        [<Fact>]
        member x.``advancePath successfully returns the sequence up till in passes the target`` () =
             Seq.unfold (advancePath testTarget) ((3, -1), (0, 0)) |> Seq.toList
             |> should equal [(0, 0); (3, -1); (5, -3)]
             
             Seq.unfold (advancePath testTarget2) ((6, 9), (0, 0)) |> Seq.toList
             |> should equal
                [(0, 0); (6, 9); (11, 17); (15, 24); (18, 30); (20, 35); (21, 39); (21, 42);
                (21, 44); (21, 45); (21, 45); (21, 44); (21, 42); (21, 39); (21, 35); (21, 30);
                (21, 24); (21, 17); (21, 9); (21, 0); (21, -10)]
             
             Seq.unfold (advancePath testTarget2) ((6,-4), (0, 0)) |> Seq.toList
             |> should equal [(0, 0); (6, -4); (11, -9)]
                         
        [<Fact>]
        member x.``the part 1 solution evaluates correctly`` () =
            day17part1solution testTarget2 |> should equal 45
            
        [<Fact>]
        member x.``the part 1 solution is`` () =
            // I find the answer by running this with a random value and seeing what the actual answer is :p 
            day17part1solution trueTarget |> should equal 9870
            
    type ``Part 2 tests`` () =
  
        let testTarget2 = { MinX = 20; MaxX = 30; MinY = -10; MaxY = -5 }       
        let trueTarget = { MinX = 119; MaxX = 176; MinY = -141; MaxY = -84 }
        
        [<Fact>]
        member x.``the part 2 solution evaluates correctly`` () =
            day17part2solution testTarget2 |> should equal 112
            
        [<Fact>]
        member x.``the part 2 solution is`` () =
            day17part2solution trueTarget |> should equal 5523 
