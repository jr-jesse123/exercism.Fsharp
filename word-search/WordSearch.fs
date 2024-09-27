module WordSearch

open System


type OptBuilder() =
    member _.Bind (x,f) = Option.bind f x
    member _.Return x = Option.Some x

let optBuilder = OptBuilder()

let findWordLeftToRight (grid: string list) (word: string) =
    grid
    |> List.mapi(fun idx line-> {| y= idx + 1; x=line.IndexOf word + 1|})
    |> List.tryFind (fun tuple -> tuple.x > 0)
    |> Option.map (fun coords -> 
        (coords.x, coords.y), (coords.x + word.Length - 1 , coords.y))

let findWordRightToLeft grid (word: string)= 
    let invertXasis ((xa, ya),(xb,yb))  =  (xb,ya) , (xa,yb)

    word.ToCharArray() 
    |> (Array.rev >> String) 
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




//findWordLeftToRight (Grid.ToLeftTopBottonRight (Array.ofList grid)) "java"
//|> Option.map(fun ((xa, ya), (xb,yb)) -> 
//   let xStream = [xa..xb]
//   let yStream = [ya..yb]
//   List.allPairs xStream yStream

//)

let findBottonToTop grid word = 
    let columGrid = 
        grid
        |> List.toArray
        |> Grid.ToColumnGrid  

    let invertXYasis ((xa, ya),(xb,yb))  =  (ya, xa) , (yb, xb)

    findWordRightToLeft columGrid word
    |> Option.map invertXYasis




let findTopLeftToBottonRight grid word = 
    let columGrid = 
        grid
        |> List.toArray
        |> Grid.ToLeftTopBottonRight
        //|> List.rev // fixes the y idx
        

    let invertXYasis ((xa, ya),(xb,yb))  =  
        let highgt = columGrid.Length
        let width = columGrid[0].Length

        let middle = (highgt + 1) / 2

        let fixX y x = 
            match y - middle with
            | offsetTop when offsetTop > 0 -> x - offsetTop
            | offSetBotton when offSetBotton < 0 -> x + offSetBotton
            | middle -> x

        let fixY x y = 
            match y - middle with
            | offsetTop when offsetTop > 0 -> 
                y + offsetTop + x - 1
            | offSetBotton -> 
                y - middle + x 
            //| middle -> x + y - 1
            //y - 9

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




