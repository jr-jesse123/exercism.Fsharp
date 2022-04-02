module LogLevels

module String = 
    let trim (str: string) = str.Trim()
    let toLower (str: string) = str.ToLower()
    let clipWord nrInitialChars nrFinalchars (str: string) =
        str.Substring(nrInitialChars, str.Length - 1 - nrFinalchars) 
open System.Text.RegularExpressions




let ``get level and message`` (log : string) = 
    match log.Split(":") with
    | [|level;message|] -> level , message
    | _ -> failwith "unknown format"


let message (logLine: string): string = 
    ``get level and message`` logLine
    |> snd
    |> String.trim


let treatLogLevel logLevel = 
    logLevel
    |> String.clipWord 1 1
    |> String.toLower

let logLevel(logLine: string): string = 
      ``get level and message`` logLine
    |> fst
    |> treatLogLevel
    
let reformat(logLine: string): string = 
    let level , message = ``get level and message`` logLine
    String.concat "" [
        
        String.trim message
        " "
        "("
        treatLogLevel level 
        ")"
    ]