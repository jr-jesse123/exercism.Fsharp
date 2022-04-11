module Pangram

let isPangram (input: string): bool = 
    let alfabet = ['a'..'z']
    let input' = input.ToLowerInvariant().ToCharArray() |> Array.toList
    alfabet |> List.except input' |> (=) []