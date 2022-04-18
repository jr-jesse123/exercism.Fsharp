module Grains

let private isBetween a b c= 
    c >= a && c <= b 

let square (n: int): Result<uint64,string> = 
    if isBetween 1 64 n then
        System.Math.Pow(2,  (float n - float 1)) 
        |> uint64
        |> Ok
    else
        Error "square must be between 1 and 64"

let total: Result<uint64,string> = 
    List.map square [1..64]
    |> List.reduce 
        (fun state item -> 
            match state , item with
            | Ok acc , Ok v -> acc + v |> Ok 
            | _ -> Error "something is wrong"
            )



