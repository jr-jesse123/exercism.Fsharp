module BeerSong


let recite (startBottles: int) (toTakeDown: int) = 
    let rec innerRecit actualBottles   = 
        match actualBottles  with 
        | 0  -> 
            [sprintf "No more bottles of beer on the wall, no more bottles of beer." 
             sprintf "Go to the store and buy some more, 99 bottles of beer on the wall." ] 
        | 1  -> 
            [sprintf "%i bottle of beer on the wall, %i bottle of beer." actualBottles actualBottles 
             sprintf "Take it down and pass it around, no more bottles of beer on the wall." ] 
        | 2 ->
            [sprintf "%i bottles of beer on the wall, %i bottles of beer." actualBottles actualBottles 
             sprintf "Take one down and pass it around, %i bottle of beer on the wall."  (actualBottles - 1) ] 
        | _ ->
            [sprintf "%i bottles of beer on the wall, %i bottles of beer." actualBottles actualBottles 
             sprintf "Take one down and pass it around, %i bottles of beer on the wall."  (actualBottles - 1) ] 

    let mutable token = 0
    let mutable actualBottles = startBottles
    let mutable recitAccumlator = []

    while token < toTakeDown do
        let recit = innerRecit actualBottles 
        recitAccumlator <-   recitAccumlator  @ [""] @ recit
        token <- token + 1
        actualBottles <- actualBottles - 1

    recitAccumlator |> List.skip 1

    