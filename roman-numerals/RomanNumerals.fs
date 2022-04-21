module RomanNumerals
let romanList =     
    [1,"I"; 5, "V"; 10, "X"; 50, "L"; 100, "C"; 500, "D"; 1000, "M"]
    |> List.rev

let romanMap = 
    romanList
    |> Map

let getResultAndRest dividend divisor : int * int  =
    let result = dividend / divisor
    result , dividend % divisor




let GetRomanGroups vlr = 
    let mutable remaining : int = vlr
    let mutable acc = []


    for idx in [0..(romanList.Length - 1)] do 

        let mutable romanValue , romanRep = romanList[idx]
        let mutable (result, newRemaining) =  getResultAndRest remaining romanValue

        let  romanRep' = 
            let nxtRomanValue = 
                try
                    Some <| fst romanList[idx + 1]
                with|ex-> None
            let (nxtResult, nxtRemaining) = 
                try
                    ( getResultAndRest newRemaining ( fst romanList[idx + 1])  )
                with|ex ->
                    0 , 0
            try
                if false then failwith ""
                else if result = 4    then (snd romanList[idx]) + (snd romanList[idx - 1]) 
                // else if result = 4  then
                //     (snd romanList[idx + 1]) + (snd romanList[idx - 1]) 
                
                else if (result = 1 && newRemaining = 4) then 
                    newRemaining <- 0
                    (snd romanList[idx + 1]) + (snd romanList[idx - 1])
                
                else if result = 0 && newRemaining.ToString().StartsWith("9") && nxtResult <> 0 && nxtRomanValue.IsSome then 
                    if (newRemaining % nxtRomanValue.Value) <> newRemaining then
                        //newRemaining <- 0//(newRemaining % 10)
                        newRemaining <- (newRemaining % 10)
                    else
                        newRemaining <- (newRemaining % 10)
                    result <- 1
                    (snd romanList[idx + 2] ) + (snd romanList[idx ])

                else romanRep
            with|ex ->
                romanRep
                

        result <-  if romanRep <>  romanRep' then 1 else result
        romanRep <- romanRep'

        remaining <- newRemaining
        acc <- (result , romanRep) :: acc
    // for (romanValue, romanRep) in romanList do 

        
        
    //     let (result, newRemaining) =  getResultAndRest remaining romanValue

        

    //     remaining <- newRemaining
    //     acc <- (result , romanRep) :: acc
    acc
    
    
let roman arabicNumeral = 
    GetRomanGroups arabicNumeral
    |> List.rev
    |> List.map (fun (repetitions, letter) -> String.replicate repetitions letter  )
    |> List.reduceBack (+)

