module OcrNumbers

type DisplayPart = |Pipe|UndersCore|Empty
type DisplayLine = DisplayPart * DisplayPart * DisplayPart
type Display = DisplayLine * DisplayLine * DisplayLine 
    
module Seq = 
    let assertLenght excpectedLenght sequence = 
        if Seq.length sequence <> excpectedLenght then failwith "unexpectedted size" else sequence
let charToDisplayPart c = 
    match c with    
    | '|' -> Pipe
    | '_' -> UndersCore
    | ' ' -> Empty
    | _ -> failwith "unexcpected char"
let rowToDisplayLine (str: string) = 
    let parts = 
        str 
        |> Seq.map charToDisplayPart 
        |> Seq.assertLenght 3
        |> Seq.toList
    DisplayLine (parts[0],parts[1],parts[2])
    
let strToDisplay str = 
        let lines = 
            str 
            |> Seq.map rowToDisplayLine
            |> Seq.assertLenght 4
            |> Seq.toList
        Display (lines[0],lines[1],lines[2])
let DisplayToStr d = 
    match  d with
    | (Empty,UndersCore, Empty) , 
      (Pipe, Empty, Pipe), 
      (Pipe,UndersCore,Pipe) ->  "0"
    
    | (Empty,Empty, Empty) , 
      (Empty, Empty, Pipe), 
      (Empty, Empty, Pipe) ->  "1"
    | (Empty,UndersCore, Empty) , 
      (Empty, UndersCore, Pipe), 
      (Pipe, UndersCore, Empty ) ->  "2"
    | (Empty,UndersCore, Empty) , 
      (Empty, UndersCore, Pipe), 
      (Empty, UndersCore, Pipe ) ->  "3"
    | (Empty,Empty, Empty) , 
      (Pipe, UndersCore, Pipe), 
      (Empty, Empty, Pipe ) ->  "4"
    
    | (Empty,UndersCore, Empty) , 
      (Pipe, UndersCore, Empty), 
      (Empty, UndersCore, Pipe ) ->  "5"
    
    | (Empty,UndersCore, Empty) , 
      (Pipe, UndersCore, Empty), 
      (Pipe, UndersCore, Pipe ) ->  "6"
    
    | (Empty,UndersCore, Empty) , 
      (Empty,Empty, Pipe), 
      (Empty, Empty, Pipe ) ->  "7"
    
    | (Empty,UndersCore, Empty) , 
      (Pipe, UndersCore, Pipe), 
      (Pipe, UndersCore, Pipe) ->  "8"
    
    
    | (Empty,UndersCore, Empty) , 
      (Pipe, UndersCore, Pipe), 
      (Empty, UndersCore, Pipe) ->  "9"
    | _ ->  "?"
open System
module Option = 
  let apply fOpt xOpt = 
    match fOpt , xOpt with  
    | Some f , x -> Some (f x)
    | _ -> None
  let retn = Option.Some
module List = 
  // let rec traverseOptionA f list = 
  //   let (<*>) = Option.apply
  //   let retn = Option.Some
  //   let cons head tail = head::tail
  //   match list with
  //   | [] -> retn []
  //   | head::tail ->
  //     retn  cons <*> (f head) <*> (traverseOptionA f tail)
    
  let rec traverseOptionM f list = 
    let (>>=) x f = Option.bind f x
    let retn = Option.Some
    let cons head tail = head::tail 
    match list with
    | [] -> retn []
    | head::tail ->
      f head >>= (fun h ->
      traverseOptionM f tail >>= (fun t ->
      retn (cons h t)))
open System
let getDigitList (input: string list) = 
    input
    |> List.map (Seq.chunkBySize 3) 
    |> List.map (Seq.map( String))  
    |> Seq.transpose 
    |> Seq.map (Seq.toList)
let convert (input: string list) =
    let convert' x = 
      try
          getDigitList x          
          |> Seq.map strToDisplay
          |> Seq.map DisplayToStr
          |> Seq.map char
          |> Seq.toArray
          |> String
          |> Some 
  
      with
      |ex -> None
      
    List.chunkBySize 4 input
    |> List.map convert'
    |> List.traverseOptionM (id)
    |> Option.map (String.concat ",")