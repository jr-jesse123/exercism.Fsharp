module RomanNumerals
open System

let placeValue   ten five one digit = 
    match digit with
    | 0 -> ""
    | 1 | 2 | 3 -> String (one, digit)
    | 4 -> sprintf "%c%c" one five
    | 5 -> sprintf "%c" five
    | 6 | 7 | 8 -> sprintf "%c%s" five (String(one,digit - 5))
    | 9 -> sprintf "%c%c" one ten
    | _ -> invalidArg "digit" "invalid digit"


let roman arabic = 
    String( 'M', arabic / 1000) + 
    placeValue 'M' 'D' 'C' (arabic % 1000 / 100) + 
    placeValue 'C' 'L' 'X' (arabic % 100 / 10) + 
    placeValue 'X' 'V' 'I' (arabic % 10)