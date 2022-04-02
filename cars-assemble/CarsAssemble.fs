module CarsAssemble

open System.Text.RegularExpressions
open System

let productionPerSpeedUnit = 221

let successRate (speed: int): float =
    match speed with
    | 0 -> 0
    | vel when vel >= 1 && vel <= 4 -> 1
    | vel when vel >= 5 && vel <= 8 -> 0.9
    | 9 -> 0.8
    | 10 -> 0.77
    | _ -> failwith "speed out of range"

let productionRatePerHour (speed: int): float =
    let rate = successRate speed 
    speed * productionPerSpeedUnit
    |> float
    |> (*) rate 
    

let dividedby divisor input = input / divisor

let workingItemsPerMinute (speed: int): int =
    let rate = 
        productionRatePerHour speed   
        |> dividedby (float 60)
    
    Math.Round (rate,0, MidpointRounding.ToNegativeInfinity) |> int
    





