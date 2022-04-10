module PigLatin

open System
let isVowel c = 
    List.contains c ["a"; "e"; "i"; "o"; "u"; "yt"; "xr"]  

let isConsoant = isVowel >> not

let startWithVoewl (word: string) = 
    word[0] |> string |> isVowel

let (|StartsWithVoewl|_|) (word:string) = 
    startWithVoewl word
    |> function |true -> Some word |false -> None

let startsWithConsoant  = 
    startWithVoewl >> not


let getAllBeginningConsoants (word: string) = 
    word 
    |> Seq.takeWhile (string >> isConsoant) 
    |> Seq.toArray
    |> String


let rec moveConsoant (word: string, consoants: string) = 
    if startsWithConsoant word then
        let consoant, rest = word[0].ToString() , word[1..]
        moveConsoant  (rest , consoants + consoant )
    else
        word + consoants


let (|StartsWithConsoant|_|) word = 
    match word with
    | StartsWithVoewl word -> None
    | _ -> Some word



// let consants = getAllBeginningConsoants "square"
// let word = "square"
// word[consants.Length..]

let (|ConsoantsWithY|_|) word = 
    let consoants = getAllBeginningConsoants word
    let rest = word[consoants.Length..]
    if rest.StartsWith("y") || rest.StartsWith("Y") then
        Some word
    else
        None
        
let word = "rhythm"        
let consoants = getAllBeginningConsoants word



let Rule1 (word: string) = 
    match word with
    | StartsWithVoewl word -> word + "ay"
    | word when word.StartsWith("qu") -> 
        word[2..] + word[0..1] + "ay"
    | StartsWithConsoant word when word[1..2] = "qu" -> 
        word[3..] + word[0..2] + "ay"
    
    | ConsoantsWithY word -> failwith "Consonants with y"
    
    | StartsWithConsoant word -> moveConsoant (word, "") + "ay"
    
    | _-> failwith "not implemented"

let translate input = Rule1 input