module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

type Command = 
|TurRight
|Advance
|TurnLeft

let comandInput = [
    'R' , TurRight
    'A', Advance
    'L' , TurnLeft
]

let getComandFromChar c = 
    comandInput |> List.find (fun (char,cmd) -> char = c) |> snd


let create direction position = {direction = direction;position = position}

let turRigh position = 
    match position with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let TurnLeft position = 
    match position with
    | North -> West
    | East -> North
    | South -> East
    | West -> South
    
let Advance  robot = // = 
    let {position = Position; direction = Direction } = robot
    let newPos = 
        match  Direction , Position  with
        | North , (xPos , yPos) -> xPos , yPos + 1
        | East , (xPos , yPos) -> xPos + 1 , yPos 
        | South , (xPos , yPos) -> xPos , yPos - 1
        | West , (xPos , yPos) -> xPos - 1, yPos 
    {robot with position = newPos}


let runCommand robot command  = 
    match command with 
    | TurnLeft -> {robot with direction = TurnLeft robot.direction}
    | TurRight -> {robot with direction = turRigh robot.direction}
    | Advance ->  Advance robot 
    

let move instructions robot = 
    
    instructions 
    |> Seq.map getComandFromChar
    |> Seq.fold runCommand robot 
    
