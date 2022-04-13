module TwelveDays


let beginning idx =
    let order = 
        match idx with  
        | 1 -> "first"
        | 2 -> "second"
        | 3 -> "third"
        | 4 -> "fourth"
        | 5 -> "fifth"
        | 6 -> "sixth"
        | 7 -> "seventh"
        | 8 -> "eighth"
        | 9 -> "ninth"
        | 10 -> "tenth"
        | 11 -> "eleventh"
        | 12 -> "twelfth"
        | _ -> failwith "only twelve days in verse"
    
    sprintf "On the %s day of Christmas my true love gave to me" order

let inputs = 
    [|
    "twelve Drummers Drumming"; "eleven Pipers Piping"; "ten Lords-a-Leaping";
    "nine Ladies Dancing"; "eight Maids-a-Milking"; "seven Swans-a-Swimming";
    "six Geese-a-Laying"; "five Gold Rings"; "four Calling Birds";
    "three French Hens"; "two Turtle Doves"; "a Partridge in a Pear Tree."|] |> Array.rev
    
let ajustLast (arr: string []) = 
        match arr with
        | [||] -> [||]
        | [|x|] -> [|x|]
        | _ ->
            let lenght = arr.Length
            arr[lenght - 1] <- "and " + (Array.last arr)
            arr    
    
    
let phrases =     
    Array.scan (fun state prhase ->   prhase :: state )  [] inputs 
    |> Array.map (List.toArray >> ajustLast)
    |> Array.map (fun phrases -> String.concat ", " phrases)
    |> Array.skip 1
    |> Array.mapi (fun  idx phrase ->  (beginning (idx + 1) + ": ") + phrase )

let recite start stop = 
    phrases[start - 1 .. stop - 1] 
    // |> Array.skip (start - 1)
    // |> Array.take (stop - start + 1)
    |> Array.toList
