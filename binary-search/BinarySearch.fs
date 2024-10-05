module BinarySearch

[<TailCall>]
let rec binarySearch input value indexOffSet =
    printfn "%A" input
    match input with
    | [||] -> None
    | [|v|] -> if value = v then Some 0 else None
    | input -> 
        let lenght = Array.length input
        let middleIndex = lenght / 2
        let middleElement = input[middleIndex]
        if middleElement = value then 
            Some (middleIndex + indexOffSet)
        elif middleElement > value then
            binarySearch input[..middleIndex - 1] value indexOffSet
        else
            let newOffset = indexOffSet + (Array.length input / 2)
            binarySearch input[middleIndex..] value newOffset
            
            
    


let find input value = 
    binarySearch input value 0