module Grep

type Grep = 
    string list ->  //files
        string list ->  //flagArguments
        string ->  //pattern
        string list //result

open System.IO
open System
open System.Runtime.CompilerServices


type FileDetails = {
    FileName: string
    Contents: (int * string) list
} with
    member this.ContainsPattern comparison (pattern:string) inverted  =
        this.Contents
        |> List.where (fun (lineNr, lineContent) -> 
            lineContent.Contains(pattern, comparison) |> inverted
            )
        



let grep : Grep = 
    fun files flagArguments pattern  ->
        let contents = 
            files
            |> List.map (fun file -> file, File.ReadAllLines(file) |> Array.toList)
            |> List.map (fun (file, lines) -> 
                 {FileName=file ; Contents=lines |> List.mapi (fun lineNr lineContent ->  lineNr, lineContent)}
            )
            
        contents
        |> List.where (fun {FileName=fileName ; Contents=contents} -> 
            match files with
            |[] -> true
            |_ -> files |> List.contains fileName
             ) 
        |> List.map (fun  fileDetails ->  
            let comparisson = if List.contains  "-i" flagArguments then StringComparison.CurrentCultureIgnoreCase else StringComparison.CurrentCulture
            (
            fileDetails.FileName, 
            fileDetails.ContainsPattern   
                comparisson 
                pattern 
                (if List.contains "-v"  flagArguments then not else id))
            )
        |> List.map (fun (fileName, contents) -> 
            if List.contains "-x"   flagArguments  then
                let comparisson = if List.contains  "-i" flagArguments then StringComparison.CurrentCultureIgnoreCase else StringComparison.CurrentCulture
                (
                    fileName, 
                    contents 
                    |> List.where (fun  (lineNr,lineContent) -> String.Equals(lineContent, pattern,comparisson) |> (if List.contains "-v"  flagArguments then not else id))
                )
            else
                (fileName, contents)
        )
        |> List.where (fun (_,lines) -> lines |> List.isEmpty |> not)
        
        |> List.map (fun (fileName, contentList) -> 
            contentList
            |> List.map (fun (lineNr, lineContent) ->
                if List.contains "-l" flagArguments then
                    fileName
                elif List.length  files > 1 &&  List.contains "-n" flagArguments |> not  then
                    sprintf "%s:%s" 
                        fileName
                        lineContent
                elif List.length  files > 1 &&  List.contains "-n" flagArguments then
                    sprintf "%s:%i:%s" 
                        fileName
                        (lineNr + 1)
                        lineContent
                else
                    sprintf "%s%s" 
                        (if flagArguments |> List.contains "-n" then sprintf "%d:" (lineNr + 1) else "")
                        lineContent
            )
        )
        |> List.concat
        |>  List.distinct //  junta nomes de arquivos
        //|> ignore
        //[]
        
        

        
        
        

        