module LargestSeriesProduct
open System


let (|WordWithNondDigit|_|) (input: string) = 
    input.ToCharArray() 
    |> Array.map (Char.IsDigit) 
    |> Array.reduce (&&)
    |> function 
        | true ->  None
        | false -> Some  input 


let largestProduct (input: string) seriesLength : int option = 
    match input, seriesLength with
    | _ , lenght when lenght < 0 -> None    
    | _ , 0 -> Some 1
    | "" , lenght when lenght > 0 -> None   
    | WordWithNondDigit sentence , _ -> None
    |   input, seriesLength when seriesLength > input.Length -> None  
    | _ ->
        input.ToCharArray()
        |> Array.map (string >> int)
        |> Array.windowed seriesLength
        |> Array.map (Array.reduce (*))
        |> function
            | [||] -> Some 1
            | values -> Array.max values |> Some
