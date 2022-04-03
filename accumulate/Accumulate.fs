module Accumulate

let cons head tail = head::tail
let (+++) = cons

let  accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec innerAccumulate func input acc = 
        match input with
        | [] -> acc 
        | [head] -> func head +++ acc
        | head::tail ->
            let newAcc = func head +++ acc  
            innerAccumulate func tail newAcc
    
    innerAccumulate func input [] |> List.rev