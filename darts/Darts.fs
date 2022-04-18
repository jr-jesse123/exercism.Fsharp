module Darts
open System


let getDistance x y =   
    (x * x) + (y * y)
    |> Math.Sqrt


let score (x: double) (y: double): int = 
    match getDistance x y with
    | distance when distance > 10 -> 0
    | distance when distance > 5 -> 1
    | distance when distance > 1 -> 5
    | distance  -> 10