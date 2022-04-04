module KindergartenGarden
type Plant = 
    | Violets
    | Radishes
    | Clover
    | Grass

let students = [
    "Alice"
    "Bob"
    "Charlie"
    "David"
    "Eve"
    "Fred"
    "Ginny"
    "Harriet"
    "Ileana"
    "Joseph"
    "Kincaid"
    "Larry"
] 

let getSills (diagram: string) = 
    match diagram.Split "\n" with
    | [|line1 ; line2|] -> [line1;line2]
    | _ -> failwith "unexpected diagram format"

let getStudentsSillPosition student = 
        students
        |> List.findIndex ((=) student) 
        |>  ((*) 2)

let mapStringToPlantList str = 
    str
    |> Seq.map (function | 'R' -> Radishes | 'C' -> Clover | 'G' -> Grass | 'V' -> Violets | _ -> failwith "unexpected char")
    |> Seq.toList
        

let substring start qty (input: string) = 
    input.Substring (start,qty)

let getStudentsSillChar student sills= 
    let startIndex =  getStudentsSillPosition student 
    sills 
    |> List.map (substring startIndex 2 )
    |> String.concat ""

let plants diagram student = 
    let sills = getSills diagram
    let chars = getStudentsSillChar student sills
    mapStringToPlantList chars

    

