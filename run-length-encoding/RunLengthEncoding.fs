module RunLengthEncoding

let splitOnDiferentChar (str:string) =
    str.ToCharArray() 
    |> Array.fold 
        (fun (acc: (char * int) list) char -> 
            match acc , char with
            | [] , char -> [ char, 1]
            | (storedChar, count)::tail , char when char = storedChar ->
                (storedChar , count + 1)::tail
            | (storedChar, count)::tail , char ->
                (char ,  1)::acc
            | _ -> invalidArg "" "" ) []
    
    
let encode input = 
    let charRepetitions =  splitOnDiferentChar input |> List.rev
    List.fold (fun acc (char,repetitions) ->
        acc + ( if repetitions = 1 then "" else string repetitions) + (string char) )  "" charRepetitions 
open System

let decode (input: string) = 
    let chars = input.ToCharArray() 
    let mutable output = ""
    let mutable lastDigit = 0
    
    for char in chars do
        if Char.IsDigit char && lastDigit = 0 then 
            lastDigit <- (string char |> int)
        else if lastDigit <> 0 && Char.IsDigit char then
            lastDigit <- (lastDigit * 10) + (string char |> int)
        else if lastDigit <> 0 && Char.IsDigit char |> not then 
            output <- output + String.replicate lastDigit (string char)
            lastDigit <- 0
        else 
            output <- output + (string char)
            lastDigit <- 0
    output

