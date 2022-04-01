module AnnalynsInfiltration

let canFastAttack (knightIsAwake: bool): bool = 
    if knightIsAwake then false else true
    //failwith "Please implement the 'canFastAttack' function"

let canSpy (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool): bool =
     knightIsAwake || archerIsAwake || prisonerIsAwake 


let canSignalPrisoner (archerIsAwake: bool) (prisonerIsAwake: bool): bool =
    match archerIsAwake,prisonerIsAwake with
    | false, true -> true
    | _ -> false

let canFreePrisoner (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) (petDogIsPresent: bool): bool =
    match knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent with
    | _ , false, _, true -> true
    | false,false,true,false -> true
    | _ -> false
