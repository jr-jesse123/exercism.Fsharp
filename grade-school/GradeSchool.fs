module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty


let add (student: string) (grade: int) (school: School): School =
    let newStudentList =
        match school.TryFind grade with
        | Some studentList -> student :: studentList
        | None -> [student]


    school.Add (grade, newStudentList |> List.sort  )

let roster (school: School): string list =
    // school.Values
    // |> List.concat

    school
    |> Map.toList
    |> List.sortBy fst
    |> List.map snd
    |> List.concat
    

let grade (number: int) (school: School): string list = 
    school.TryFind number
    |> Option.defaultValue [] 