module WordSearch

open System.Reflection
open System

(*
    specs:
     - Receber uma lista de strings e uma lista de palavra alvo a ser localizada
     - Retornar um dicion�rio comas a(s) palavras alvo localizzadas 
     - as palavras podem estar da esquerda para direita ou direita pra esquer
     - as palavras podem estar escritas de cima pra baixo ou de baixo pra cima
     - as palavras podem estar na diagonal da esquerda superior para a direita inferior ou inverso disso.
     - as palavaras podem estar na diagonal da esquerda inferior para a direita superior ou inverso disso
*)

(*
    steps: 
        Receber duas listas de string, uma contendo o puzzle e a outra contendo as palavras alvo
        mapear o puzzle para uma lista de letras com posicoes
        ler as letras com posi��es em todas as dire��es
        comparar estas novas linhas com as palavras alvo e retornar as posi��es se localizadas
*)


type YPosition = | YPosition of int
with member this.value = match this with YPosition y -> y

type XPosition = | XPosition of int
with member this.value = match this with XPosition x -> x

type Position = {X:XPosition;Y:YPosition}
type PositionedChar = {Char:char;Position:Position}
type SpanPosition = {Start:Position; End:Position}

type PositionedString = {Str: string; SpanPosition: SpanPosition }

type CharGrid = PositionedChar list list 
type WordPosition = Position * Position


type ToPositionedChars = string list -> CharGrid

type GetWordsInAllDirections = CharGrid -> CharGrid list 

type MatchTargetWords = CharGrid -> string list -> Map<string, WordPosition option>

type GetStrings = CharGrid -> string list -> (PositionedString) list

type SortGrid = CharGrid -> CharGrid

type ToString = PositionedChar list -> string
//use case
type Search = CharGrid -> string list -> Map<string, (int * int) * (int * int) option>

//implementation
let toPositionedChars : ToPositionedChars = 
    fun (input) ->
        input 
        |> List.mapi (fun yIdx line -> 
            line.ToCharArray() 
            |> Array.mapi (fun xIdx char ->  { Char=char; Position= {X= XPosition xIdx; Y= YPosition yIdx} } )
            |> Array.toList
        )

//let CharGrid = toPositionedChars grid

let makeTopDownGrid : SortGrid =
    fun grid ->
        List.concat grid
        |> List.sortBy(fun char -> char.Position.Y) 
        |> List.sortBy (fun char -> char.Position.X)   
        |> List.groupBy (fun char -> char.Position.Y) 
        |> List.map snd
        


let makeTopLeftBottonDownGrid = 
    //let isSameLine x y = 
        
    fun grid ->
        List.concat grid
        |> List.sortBy(fun char -> char.Position.Y.value + char.Position.X.value) 
        |> List.sortBy (fun char -> char.Position.X)   
        |> List.fold (fun state currChar ->  //this should not bbe a list
            let previousLineOpt = state |> List.tryLast
            match previousLineOpt with
            | None -> [[currChar]]
            | Some (previousLine)-> 
                if (List.last previousLine |> _.Position.X) >  currChar.Position.X then
                    
                        (previousLine @ [ currChar ]) :: state[..state.Length - 2]
                        |> List.rev
                        
                else
                    [ currChar ] :: state
                    |> List.rev
                   

            //state
            ) []
        //|> List.map snd
        
    
let makeBBottonLeftTopUpGrid = 
    
     fun grid ->
         List.concat grid
         |> List.sortBy(fun char -> char.Position.Y.value + char.Position.X.value) 
         |> List.sortBy (fun char -> char.Position.X)   
         |> List.fold (fun state currChar ->  //this should not bbe a list
             let previousLineOpt = state |> List.tryLast
             match previousLineOpt with
             | None -> [[currChar]]
             | Some (previousLine)-> 
                 if (List.last previousLine |> _.Position.X) <  currChar.Position.X then
                     
                         (previousLine @ [ currChar ]) :: state[..state.Length - 2]
                         |> List.rev
                         
                 else
                     [ currChar ] :: state
                     |> List.rev
                    

             //state
             ) []

let toString: ToString = 
    fun charList ->
        charList |> List.map _.Char |> List.toArray |> String

let getString  : GetStrings =
    fun (grid: CharGrid) (words: string list) ->
       grid
       |> List.map (fun line ->
            let text = toString line
            words |> List.map (fun word -> 
                let idx = text.IndexOf word 
                if idx > -1 then 
                    let pos = {Start=line[idx].Position;End=line[idx + word.Length - 1].Position}
                    Some {Str=word; SpanPosition = pos} 
                else
                    None
                )
       )
       |> List.map (fun x -> x |> List.choose id)
       |> List.concat
       //[]

let getWordsInAllDirections : GetWordsInAllDirections =
    fun originalGrid ->
        [
            id // left to right
            //List.rev //right to left
            makeTopDownGrid
            makeBBottonLeftTopUpGrid 
            
        ]
        |> List.map (fun func -> func originalGrid)
        

let search' : Search = 
    fun grid searchedWords ->
        let allGrids = getWordsInAllDirections grid


        Map.empty
    



let search grid wordsToSearchFor: Map<string,((int * int) * (int * int)) option>    =
    List.map (fun x -> x, None) wordsToSearchFor
    |> Map.ofList

