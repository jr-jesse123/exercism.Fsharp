module BinarySearch
open System


// let (|Bigger|Smaller|Equal|) (y: int) value = 
//     if value > y then Bigger 
//     elif value < y then Smaller 
//     else Equal 


let (|Bigger|_|)  (y: int) value = 
    if value > y then Some() else None

let (|Smaller|_|)  (y: int) value = 
    if value < y then Some() else None

let (|Equal|_|)  (y: int) value = 
    if value = y then Some() else None


let imperativeFind input target = 
    let input = Seq.toArray input
    let mutable left , right = 0 , input.Length

    while left < right do
        let mid = left + (right - left) / 2        
        match input[mid] with
        |Smaller target -> left <- mid + 1
        | _ -> right <- mid

    if left <> input.Length && input[left] = target then Some left else None


[<Literal>]
let LT = -1
[<Literal>]
let GT = 1

let functionalFind (input: int array) target = 
    let rec find (input: int array) left right = 
        match left >= right with
        | true -> None
        | false -> 
            let midPointer = left + (right - left) / 2
            let midValue = input[midPointer]
            match compare target midValue  with
            | GT -> find input (midPointer + 1)  right
            | LT -> find input left midPointer
            | 0 -> Some midPointer
            | _ -> failwith "not allowed to be here"

    find input 0 input.Length
    


let find (input: int array) target = 
    
    functionalFind input target
    // let rec find input left right = 
    //     let midValue = right - left / 2
    //     match input[]
    
    // match input with
    // | [||] -> None
    // | [|x|] -> Some 0
    // | input -> find 0 input.Length

    



