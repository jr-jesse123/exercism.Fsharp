module Raindrops

let hasFactor factor input  = input % factor = 0

let factorsAndOutputs = [
    hasFactor 3 , "Pling"
    hasFactor 5 , "Plang"
    hasFactor 7 , "Plong"
]

let convertFactors number = 
    factorsAndOutputs 
    |> List.map (fun (hasFactor , output) -> if hasFactor number then output else "")
    |> List.fold (+) ""
    

let convert (number: int): string = 
    match convertFactors number with
    | "" -> string number
    | converted -> converted
