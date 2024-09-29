module WordSearch

open System


type OptBuilder() =
    member _.Bind (x,f) = Option.bind f x
    member _.Return x = Option.Some x

let optBuilder = OptBuilder()

type coors = {x:int;y:int}

type charWithPos = 
    {
        char: char
        pos: coors
    }




let findWordLeftToRight (grid: string list) (word: string) =
    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = {x=xidx;y=ydx}  })
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
    |> Option.map (fun ({x=xa;y=ya},{x=xb;y=yb}) ->
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

let ToLeftTopBottonRight  (edges: coors seq) (grid: 'a array array)= 
        [
            for edge in edges do 
                
                    yield 
                        [|
                            let mutable {x=x ; y=y} = edge
                            
                            while y < grid.Length && x < grid[0].Length do
                                yield grid[y][x]
                                x  <- x + 1 
                                y <- y + 1
                        |]

        ]
        |> List.toArray


let toCoors (x,y) = {x=x;y=y}

let findTopLeftToBottonRight (grid: string list) (word: string) = 
    let edges = 
            [for a in 0..9 do yield a, 0] @
            [for a in 0..9 do yield 0, a] 
            |> List.distinct
            |> List.map  toCoors
        
    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = {x=xidx;y=ydx}})
            )
    
    originalPositions
    |> List.toArray
    //|> Array.map (_.ToCharArray())
    |> ToLeftTopBottonRight edges
    |> Array.tryFind (fun lineArray ->
        lineArray |> Array.map _.char |> String |> _.Contains(word)
    )
    |> Option.map (fun lineArray -> 
        let start , lenght =
            lineArray |> Array.map _.char |> String |> _.IndexOf(word) , word.Length

        lineArray[start].pos, lineArray[start + lenght - 1].pos )
    
    |> Option.map (fun ({x=xa;y=ya}, {x=xb;y=yb}) ->
        ((xa + 1 ,ya + 1), (xb + 1,yb + 1)) 
    )


let findBottonRightToTopLeft (grid: string list) (word: string) = 
    let edges = 
            [for a in 0..9 do yield a, 0] @
            [for a in 0..9 do yield 0, a] 
            |> List.distinct
            |> List.map toCoors
        
    let word = word.ToCharArray() |> Array.rev |> String



    
    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = {x=xidx;y= ydx}})
            )

    originalPositions
    |> List.toArray
    |> ToLeftTopBottonRight edges
    |> Array.tryFind (fun lineArray ->
        lineArray |> Array.map _.char |> String |> _.Contains(word)
    )
    |> Option.map (fun lineArray -> 
        let start , lenght =
            lineArray |> Array.map _.char |> String |> _.IndexOf(word) , word.Length

        lineArray[start + lenght - 1].pos, lineArray[start].pos )
    
    |> Option.map (fun ({x=xa;y=ya}, {x=xb;y=yb}) ->
        ((xa + 1 ,ya + 1), (xb + 1,yb + 1)) 
    )

    

let ToBottonLeftToTopRight (edges: coors seq)  (grid: 'a array array): 'a array array= 
    //let edges = 
    //        [for a in 0..9 do yield 0, a] @
    //        [for a in 0..9 do yield a, 9] 
    //        |> List.distinct

    //grid[0][9]
    if grid.Length <> grid[0].Length then
        [||]
    else
        [
            for edge in edges do 
                    yield 
                        [|
                            let mutable {x=x;y=y} = edge
                        
                            while y >= 0 && x < grid[0].Length do
                                yield grid[y][x]
                                
                                x <- x + 1 
                                y <- y - 1
                        |]

        ]
        |> List.toArray



let ToRightToBottomLeft (edges: coors seq)  (grid: 'a array array): 'a array array= 
    if grid.Length <> grid[0].Length then
        [||]
    else
        [
            for edge in edges do 
                    yield 
                        [|
                            let mutable {x=x;y=y} = edge
                        
                            while y >= 0 && y < grid.Length  && x >= 0 && x < grid[0].Length do
                                yield grid[y][x]
                                
                                x <- x - 1 
                                y <- y + 1
                        |]

        ]
        |> List.toArray
    
        

let findBottonLeftToTopRight (grid: string list) (word: string) = 
    let edges = 
            [for a in 0..9 do yield a, 9] @
            [for a in 0..9 do yield 0, a] 
            |> List.distinct
            |> List.map (fun (x,y) -> {x=x;y=y})
        
    
    
    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = {x=xidx;y= ydx}})
            )

    
    originalPositions
    |> List.toArray
    |> ToBottonLeftToTopRight edges 
    |> Array.tryFind (fun lineArray ->
        lineArray |> Array.map _.char |> String |> _.Contains(word)
    )
    |> Option.map (fun lineArray -> 
        let start , lenght =
            lineArray |> Array.map _.char |> String |> _.IndexOf(word) , word.Length

        lineArray[start].pos, lineArray[start + lenght - 1].pos )
    
    |> Option.map (fun ({x=xa;y=ya}, {x=xb;y=yb}) ->
        ((xa + 1 ,ya + 1), (xb + 1,yb + 1)) 
    )

let findTopRightTobottomLeft (grid: string list) (word: string) = 
    let edges = 
            [for a in 0..9 do yield a, 0] @
            [for a in 0..9 do yield 9, a] 
            |> List.distinct
            |> List.map (fun (x,y) -> {x=x;y=y})
        

    
    let originalPositions = 
        grid
        |> List.mapi(fun ydx line-> 
            line.ToCharArray() |> Array.mapi (fun xidx c -> {char=c;pos = {x=xidx;y= ydx}})
            )

    //let word = word.ToCharArray() |> Array.rev |> String
    originalPositions
    |> List.toArray
    |> ToRightToBottomLeft edges 
    //|> Array.map (Array.map _.char)
    |> Array.tryFind (fun lineArray ->
        lineArray |> Array.map _.char |> String |> _.Contains(word)
    )
    |> Option.map (fun lineArray -> 
        let start , lenght =
            lineArray |> Array.map _.char |> String |> _.IndexOf(word) , word.Length

        lineArray[start].pos, lineArray[start + lenght - 1].pos )
    
    |> Option.map (fun ({x=xa;y=ya}, {x=xb;y=yb}) ->
        ((xa + 1 ,ya + 1), (xb + 1,yb + 1)) 
    )

module Option = 
    //let Apply fOpt xOpt = 
    //    match fOpt , xOpt with
    //    | Some f, x -> Some (f x)
    //    | _ -> None
    let Or xOpt YOpt =
        match xOpt, YOpt with
        | Some x, Some y -> Some x
        | Some x, None -> Some x
        | None , Some y -> Some y
        | None, None -> None

let (or) = Option.Or

let findWords grid word = 
    [
        word, findWordLeftToRight grid word or
            findWordRightToLeft grid word or
            findTopToBotton grid word or
            findBottonToTop grid word or
            findTopLeftToBottonRight grid word or
            findBottonRightToTopLeft grid word or
            findBottonLeftToTopRight grid word or
            findTopRightTobottomLeft grid word
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




