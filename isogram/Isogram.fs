module Isogram

open System
let ischarTwice (maybeChar, length) =
    match Char.IsLetter maybeChar , length with
    | true , l when l > 1 -> true
    | _ -> false

let isIsogram (str: string) = 
    str.ToLower().ToCharArray() 
    |> Seq.toList
    |> List.groupBy id
    |> List.map (fun (g,c) -> g , c.Length )
    |> List.filter ischarTwice
    |> List.length
    |> (=) 0

