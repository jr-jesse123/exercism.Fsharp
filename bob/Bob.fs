module Bob
open System

let IsAllUpperCase (str: string) = 
    let str = str |> Seq.filter Char.IsLetter |> Seq.toArray |> String
    if String.IsNullOrWhiteSpace str then
        false
    else
        str    
        |> Seq.map Char.IsUpper  
        |> Seq.fold (&&) true

let (|Nothing|_|) input = 
    if String.IsNullOrWhiteSpace input then Some input else None

let (|Shouting|_|) input = 
    if IsAllUpperCase input then Some input else None

let (|Question|_|) (input: string ) =
    if input.Trim().EndsWith '?' then Some input else None

let (|YeldeddQuestion|_|) (input: string) = 
    match input, input with
    | Question isquestion , Shouting isShoutin -> Some input
    | _ -> None
    

let response (input: string): string = 
    match input with 
    | YeldeddQuestion input -> "Calm down, I know what I'm doing!"
    | Nothing input -> "Fine. Be that way!"
    | Shouting input -> "Whoa, chill out!"
    | Question input -> "Sure."
    | _ -> "Whatever."
//    | 