module TisburyTreasureHunt

let getCoordinate (line: string * string): string =
    snd line

open System


let convertCoordinate (coordinate: string): int * char = 
    match coordinate.ToCharArray() with
    |[|c1;c2|] when Char.IsDigit c1 ->  c1.ToString() |> int , c2
    | _ -> failwith "incorrect format" 

let compareRecords (azarasData: string * string) ((locName , ruisCoord, quadrant): string * (int * char) * string) : bool = 
    let azanaCoord = azarasData |> snd |> convertCoordinate
    azanaCoord = ruisCoord

let fst' = fun (a,b,c) -> a
let snd' = fun (a,b,c) -> b
let trd' = fun (a,b,c) -> c
let createRecord ((azarasData): string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    if compareRecords azarasData ruisData then
        snd azarasData, fst' ruisData,trd' ruisData, fst azarasData
    else
        "", "", "", ""