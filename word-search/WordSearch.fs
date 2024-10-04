module WordSearch

open System.Reflection
open System
open System.Runtime.InteropServices

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

type PositionedString = {Str: string; SpanPosition: SpanPosition option }

type CharGrid = PositionedChar list 
type WordPosition = Position * Position


type ToPositionedChars = string list -> CharGrid

type GetWordsInAllDirections = CharGrid -> CharGrid list 

type MatchTargetWords = CharGrid -> string list -> Map<string, WordPosition option>

type GetStrings = string list -> CharGrid -> PositionedString list

type SortGrid = CharGrid -> CharGrid

type ToString = PositionedChar array -> string
//use case
type Search = CharGrid -> string list -> Map<string, ((int * int) * (int * int)) option>

//implementation
let toPositionedChars : ToPositionedChars = 
    fun (input: string list) ->
        input 
        |> List.mapi (fun yIdx line -> 
            line.ToCharArray() 
            |> Array.mapi (fun xIdx char ->  { Char=char; Position= {X= XPosition xIdx; Y= YPosition yIdx} } )
        )
        |> Array.concat
        |> Array.toList

let makeTopDownGrid : SortGrid =
         List.sortBy(fun char -> char.Position.Y) 
        >> List.sortBy (fun char -> char.Position.X)   
       
let makeTopLeftBottonDownGrid : SortGrid= 
    List.sortBy (fun c -> c.Position.X.value - c.Position.Y.value)
        
let makeBBottonLeftTopUpGrid : SortGrid = 
      List.sortBy(fun char -> (-char.Position.Y.value * 1000) + (-char.Position.X.value * 1000)) 
        
let toString: ToString = 
    fun charList ->
        charList |> Array.map _.Char |>  String

let getStrings  : GetStrings =
    let isContinuousChar previousChar currChar =
        let previousPosition = previousChar.Position
        let currentPosition = currChar.Position
        let isSameLine = previousPosition.Y = currentPosition.Y
        let isSameColumn = previousPosition.X.value = currentPosition.X.value 
        let isDiagonal = 1 = abs (previousPosition.Y.value - currentPosition.Y.value ) && 1 = abs (previousPosition.X.value - currentPosition.X.value)
        isSameLine || isSameColumn || isDiagonal
        
    let folder acc currChar =
        match acc with
        |[||] -> [|[|currChar|]|]
        | acc ->
            let lastLine = Array.last acc
            if isContinuousChar (Array.last lastLine) currChar then
                Array.append acc[..acc.Length - 2]   [|Array.append   lastLine  [|currChar|]|]
            else
                Array.append acc  [|[|currChar|]|]

    let getPositionedStrings  (words: string list) (line :PositionedChar array)=
        let text = toString line
        words 
        |> List.map (fun word -> 
            let idx =  text.IndexOf word
            if idx > -1 then 
                let pos = {Start= line[idx].Position; End=line[idx + word.Length - 1].Position}
                {Str=word;SpanPosition=Some pos}
            else
                {Str=word;SpanPosition=None}
        )

    fun words grid ->
        let getPositionedStrings'= getPositionedStrings words
        let x= 
            grid
            |> List.fold folder [||]
            |> Array.map getPositionedStrings'
        
        List.concat x
        
let getWordsInAllDirections : GetWordsInAllDirections =
    fun originalGrid ->
        [
            id // left to right
            List.rev //right to left
            makeTopDownGrid
            makeTopDownGrid >> List.rev
            makeTopLeftBottonDownGrid
            makeTopLeftBottonDownGrid >> List.rev
            makeBBottonLeftTopUpGrid 
            makeBBottonLeftTopUpGrid >> List.rev
        ]
        |> List.map (fun func -> func originalGrid)
        

let search' : Search = 
    let spanToDto (spanPos: SpanPosition ) =
        spanPos
        |>  (fun {Start=start;End=``end``} -> 
            (start.X.value + 1, start.Y.value + 1), (``end``.X.value + 1, ``end``.Y.value + 1 )
        )

    fun grid wordsToSearchFor ->
        let getPositionedStrings = getStrings wordsToSearchFor 
        getWordsInAllDirections grid
        |> List.map getPositionedStrings
        |> List.concat
        |> List.map (fun positionedStr -> 
            positionedStr.Str, positionedStr.SpanPosition |> Option.map spanToDto 
        )
        |> List.groupBy fst
        |> List.map (fun (key, group) -> List.sortBy (snd >> Option.isSome) (group))
        
        |> List.concat
        |> Map.ofList
        

let search grid wordsToSearchFor: Map<string,((int * int) * (int * int)) option>    =
    search' (toPositionedChars grid) wordsToSearchFor




