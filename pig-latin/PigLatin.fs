module PigLatin

let vowels =  ["a"; "e"; "i"; "o"; "u";] 
let vowels' =  ["a"; "e"; "i"; "o"; "u"; "y";] 

open System
let isVowel  vowelsList c = 
    List.contains c  vowelsList

let isConsoant vowelsList = (isVowel vowelsList) >> not

let startWithVoewl (word: string) = 
    word[0] |> string |> isVowel vowels

let (|StartsWithVoewl|_|) (word:string) = 
    startWithVoewl word
    |> function |true -> Some word |false -> None

let startsWithConsoant  = 
    startWithVoewl >> not


let getAllBeginningConsoants (word: string) = 
    if startsWithConsoant word then
        word
        |> Seq.skip 1 
        |> Seq.takeWhile (string >> isConsoant vowels') 
        |> Seq.toArray
        |> String
        |> (+) (string word[0])
    else
        ""

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



let translateWord (word: string) = 
    match word with
    |  word when word[1] = 'y' -> 
        word |> Seq.rev |> Seq.toArray |> String 
        |> fun x -> x + "ay"
    

    |  word when word[0..1] = "xr" || word[0..1] = "yt"  -> 
        word + "ay"
    
    | StartsWithVoewl word -> word + "ay"
    
    | word when word.StartsWith("qu") -> 
        word[2..] + word[0..1] + "ay"

    
    | ConsoantsWithY word -> 
        let sndPart = getAllBeginningConsoants word
        let fstParts = word[sndPart.Length..]
        String.concat "" [fstParts;sndPart;"ay"]
    | StartsWithConsoant word when word[1..2] = "qu" -> 
        word[3..] + word[0..2] + "ay"
    
    
    | StartsWithConsoant word -> moveConsoant (word, "") + "ay"
    
    | _-> failwith "not implemented"

let translate (input: string) = 
    input.Split " "
    |> Array.map translateWord
    |> String.concat " "