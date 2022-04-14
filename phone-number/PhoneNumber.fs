module PhoneNumber
open System

let removeChars charsSeq input = 
    Seq.filter (fun c -> Seq.contains c charsSeq |> not) input
    |> Seq.toArray 
    |> String

let checkInputLenght (input: string) = 
    match input.Length with
    | x when x > 11 ->  Error "more than 11 digits"
    | 11 when input.StartsWith '1' -> Ok input[1..]
    | 11 -> Error "11 digits must start with 1"
    | 10 -> Ok input
    | _ -> Error "incorrect number of digits"
        

    

let assertBool   message value bool=
    match bool with 
    |false -> Ok value
    |true -> Error message

let AssertNoLetters input = 
    Seq.exists (fun c -> Char.IsLetter c) input
    |> assertBool "letters not permitted" input

let AssertNoPunctuation input = 
    Seq.exists (fun c -> Char.IsPunctuation c) input
    |> assertBool "punctuations not permitted" input

let checkNumbers (input: string) = 
    match input[..2],input[3..5],input[6..] with
    | area, _ , _ when area.StartsWith '0'  -> Error "area code cannot start with zero"
    | area, _ , _ when area.StartsWith '1'  -> Error "area code cannot start with one"
    | _, exchange , _ when exchange.StartsWith '0'  -> Error "exchange code cannot start with zero"
    | _, exchange , _ when exchange.StartsWith '1'  -> Error "exchange code cannot start with one"
    | _ -> Ok input
    


let clean (input: string) = 
    removeChars [' '; '-'; '.';'(';')';'+'] input 
    |> AssertNoLetters
    |> Result.bind AssertNoPunctuation
    |> Result.bind checkInputLenght
    |> Result.bind checkNumbers
    |> Result.map uint64
    

