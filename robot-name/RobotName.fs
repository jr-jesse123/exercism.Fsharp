module RobotName

open System

type Robot = 
    {Name: string}

let rnd = Random()

let rndNames = 
    let a = [|'A'..'Z'|] 
    let b = [|'A'..'Z'|] 
    let c = [|0..999|]
    rnd.Shuffle a; rnd.Shuffle  b   
    
    let letters = Seq.allPairs a b
    Seq.allPairs letters c

let mutable currentName = 0
let nextName ()  =
    let name = rndNames |> Seq.item currentName
    currentName <- currentName + 1
    name

    
let mkRobot() = 
    let format = "000"
    let tag = 
        nextName()
        |> fun ((la,lb), n) -> $"{la}{lb}{n.ToString(format)}"
    {Name=  tag}

let name {Name=name} = name

let reset robot = 
    mkRobot()