module WordSearch

let search grid wordsToSearchFor: Map<string,((int * int) * (int * int)) option>    =
    List.map (fun x -> x, None) wordsToSearchFor
    |> Map.ofList

