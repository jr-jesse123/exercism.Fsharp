module WordSearch

open System


type OptBuilder() =
    member _.Bind (x,f) = Option.bind f x
    member _.Return x = Option.Some x

let optBuilder = OptBuilder()

type charWithPos = 
    {
        char: char
        pos: int * int
    }




let findWordLeftToRight (grid: string list) (word: string) =
    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = xidx, ydx})
            )
    
    originalPositions
    |> List.map (Array.map (fun x -> x.char)  ) 
    |> List.map String
    |> List.mapi (fun y line -> (line.IndexOf word ) , y )
    |> List.tryFind (fun (x, y) -> x >= 0)
    |> Option.map (fun (x,y) -> 
        let foundPositons = 
            [
                for pos in x..(x + word.Length - 1) do
                    yield originalPositions[y][pos]
            ]       
        foundPositons.Head.pos , foundPositons |> List.last |> _.pos
        )
    |> Option.map (fun ((xa,ya), (xb,yb)) ->
        ((xa + 1 ,ya + 1), (xb + 1,yb + 1)) 
    )

    //|> List.tryFind (fun tuple -> tuple.x > 0)
    //|> Option.map (fun coords -> 
    //    (coords.x, coords.y), (coords.x + word.Length - 1 , coords.y))

let findWordRightToLeft grid (word: string)= 
    let invertXasis ((xa, ya),(xb,yb))  =  (xb,ya) , (xa,yb)

    word.ToCharArray() 
    |> (Array.rev >> String)
    //|> (Array.rev >> String) 
    |> findWordLeftToRight grid 
    |> Option.map invertXasis

module Grid = 
    let ToColumnGrid (grid:string array) = 
        [
            let lineLenght = grid[0].Length
            for colPos in 0..(lineLenght - 1) do
                let col = 
                    [|for line in grid do 
                        yield line[colPos]|]
                yield  String col
        ]
    
    let ToLeftTopBottonRight (grid: string array) = 
        
            [
                for offset in 0..(grid.Length - 1) do 
                     yield   
                        String.replicate (grid.Length - offset - 1) " " + 
                        grid[offset]  + 
                        String.replicate offset " "
            ]
            |> List.toArray
            |> ToColumnGrid








let findTopToBotton grid word = 
    let columGrid = 
        grid
        |> List.toArray
        |> Grid.ToColumnGrid  

    let invertXYasis ((xa, ya),(xb,yb))  =  (ya, xa) , (yb, xb)

    findWordLeftToRight columGrid word
    |> Option.map invertXYasis



let findBottonToTop grid word = 
    let columGrid = 
        grid
        |> List.toArray
        |> Grid.ToColumnGrid  

    let invertXYasis ((xa, ya),(xb,yb))  =  (ya, xa) , (yb, xb)

    findWordRightToLeft columGrid word
    |> Option.map invertXYasis




let findTopLeftToBottonRight (grid: string list) (word: string) = 
    let edges = 
            [for a in 0..9 do yield a, 0] @
            [for a in 0..9 do yield 0, a] 
            |> List.distinct
        


    let ToLeftTopBottonRight (grid: 'a array array) = 
            [
                for edge in edges do 
                    
                        yield 
                            [|
                                let mutable (x, y ) = edge
                                
                                while y < grid.Length && x < grid[0].Length do
                                    yield grid[y][x]
                                    x  <- x + 1 
                                    y <- y + 1
                            |]
               
                //for offset in 0..(grid.Length - 1) do 
                //     yield   
                //        String.replicate (grid.Length - offset - 1) " " + 
                //        grid[offset]  + 
                //        String.replicate offset " "
            ]
            |> List.toArray


    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = xidx, ydx})
            )
    
    grid
    |> List.toArray
    |> Array.map (_.ToCharArray())
    |> ToLeftTopBottonRight
    |> Array.map String
    |> Array.mapi (fun y line -> (line.IndexOf word ) , y )
    |> Array.tryFind (fun (x, y) -> x >= 0)
    |> Option.map (fun (x,y) -> (x,y) , (x + word.Length - 1,y))
    |> Option.map (fun ((xa,ya), (xb,yb)) ->
        ((xa ,ya ), (xb ,yb + xb )) 
    )
    |> Option.map (fun ((xa,ya), (xb,yb)) ->
        ((xa + 1 ,ya + 1), (xb + 1,yb + 1)) 
    )

let  findTopBottonRightToTopLeft grid word =
    let columGrid = 
        grid
        |> List.toArray
        |> Grid.ToLeftTopBottonRight
        |> List.map Seq.rev
        |> List.map Array.ofSeq
        |> List.map String
        

    let invertXYasis ((xa, ya),(xb,yb))  =  
        let highgt = columGrid.Length
        let width = columGrid[0].Length

        let middle = (highgt + 1) / 2

        let fixX y x = 
            //match y - middle with
            //| offSetBottom when offSetBottom > 0 -> x - offSetBottom
            //| offSetTop when offSetTop < 0 -> x + offSetTop
            //| middle -> x
            //match x with
            //| xx when xx > 5 ->
            //    Math.Abs(xx - 10)
            //| xx -> 

            //x
            Math.Abs(x - 10)

        let fixY (x:int) y = 
            let x = fixX y x
            let emptSpaces = 
                match y with
                |y' when y' > 9 -> 1
                |y' when y' < 9 -> y' + 9
                | y' -> y'

            let firstCharY =
                Math.Abs (y - 11 )

            firstCharY + x - 1
                
                
            
            
            

        (fixX ya xa, fixY xa ya) , (fixX yb xb, fixY xb yb)

    findWordLeftToRight columGrid word
    |> Option.map invertXYasis        


let findWords grid word = 
    [
        word, findWordLeftToRight grid word
        word, findWordRightToLeft grid word
        word, findTopToBotton grid word
        word, findBottonToTop grid word
        word, findTopLeftToBottonRight grid word
        word, findTopBottonRightToTopLeft grid word
    ]
    |> List.filter (snd >> Option.isSome)
    |> List.tryHead
    |> Option.defaultValue (word, None)
    //|> function
    //    | [] -> [word, None] 
    //    | x -> x


let search grid wordsToSearchFor: Map<string,((int * int) * (int * int)) option>    =
    let findWords = findWords grid
    let x = 
        wordsToSearchFor
        |> List.map (fun x -> findWords  x ) 
        |> Map.ofList
    x




