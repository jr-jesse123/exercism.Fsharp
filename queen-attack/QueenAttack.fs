module QueenAttack

let isBetween0and7Index x = 
    x >= 0 && x<=7

let isBtwen = isBetween0and7Index


let create (position: int * int) = 
    match position with
    | x,y when isBtwen x && isBtwen y -> true
    | _ -> false

let hasSameRow (a,_) (b,_) = a = b
let hasSameColum (_,a) (_,b) = a = b

let hasSameDiagonal (a,b) (c,d) = 
    match a - c , b - d with
    |x,y when x = y -> true
    | _ -> a + b = c + d

let conditions = [
    hasSameColum
    hasSameRow
    hasSameDiagonal
]

let canAttack (queen1: int * int) (queen2: int * int) = 
    conditions |> List.map (fun f -> f queen1 queen2) |> List.reduce (||)