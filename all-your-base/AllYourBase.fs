module AllYourBase
open System
let NoneIfNegative intList = 
    List.exists (fun n -> n < 0) intList
    |> function
    | true -> None
    | false -> Some intList


let NoneIfDigitOutOfRange min max intList =
    List.exists (fun x -> x > max - 1 || x < min) intList
    |> function | true -> None | false -> Some intList


let SumDigits bbase =
    List.rev 
        >> List.mapi (fun idx n -> ( n) * (pown bbase idx ) )
        >> List.fold (+) 0

let getTotal bbase= 
    NoneIfDigitOutOfRange 0 bbase
    >> Option.map (SumDigits bbase)
        
let getDigitsFromValue outputBase total = 
      List.unfold (fun state -> 
            if (state = 0) then None
            else Some(state % outputBase, state / outputBase)) total
        |> List.rev


let rebase (digits: int list) inputBase outputBase =
    // digits
    match getTotal inputBase digits, inputBase , outputBase with
    | _ , inputBase , outputBase  when
         outputBase < 2 || inputBase < 2  -> None
    | Some 0 , _ , _ -> Some [0]
    | total , _ , _ -> 
        Option.map (getDigitsFromValue outputBase) total


        