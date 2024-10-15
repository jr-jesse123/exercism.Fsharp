module Grep
open System.IO
open System

(*
 flags: 
    filtering: -v (invert)   -i  (case insensitive)    -x (whole line)
    formatting: -l (filename) -n (lineNumber)
*)
[<Flags>]
type FilterFlags = 
    | V =1
    | I =2
    | X= 4

module FilterFlags = 
    let from char = 
            match char with
            | 'v' -> FilterFlags.V
            | 'i' -> FilterFlags.I
            | 'x' -> FilterFlags.X

[<Flags>]
type FormatFlags = 
    | L = 1
    | N = 2

module FormatFlags = 
    let from char = 
            match char with
            | 'l' -> FormatFlags.L
            | 'n' -> FormatFlags.N
            

type GrepArgs = {
    FileNames: string list
    FilterFlags : FilterFlags
    FormatFlags: FormatFlags
    Pattern: string
}

type FileContent = {
    FileName: string
    FileContent: string list
}

type GetFileContents = 
    unit  -> //path
        FileContent[]

module FileContent = 
    let get : GetFileContents =
        fun () ->
            Directory.GetFiles("./") 
            |> Array.map (fun path' -> 
                let content = File.ReadAllLines path' |> Array.toList
                {FileName= Path.GetFileName path'; FileContent=content}
            )

type MatchResult = {
    FileName: string
    LineNumber: int
    LineContent: string
}

type Search = GrepArgs -> FileContent list -> MatchResult list

let search : Search =
        
    fun args files ->
        let filteredBy f = 
            args.FilterFlags &&& f = f

        let equals = fun c a b  -> String.Equals(a,b,c)
        let contains  = fun c (a: string) (b: string)  -> a.Contains(b,c)
        let caseMode = if filteredBy FilterFlags.I then StringComparison.CurrentCultureIgnoreCase else StringComparison.CurrentCulture
        let wideMode = if filteredBy FilterFlags.X then equals  else contains
        let (|=|) =  (wideMode caseMode)
        let invertMod = if filteredBy FilterFlags.V then not else id
        
        files
        |> List.where (fun (fc: FileContent )-> match args.FileNames with |[] -> true |_ ->  args.FileNames |> List.contains fc.FileName )
        |> List.map (fun fc -> 
            fc.FileContent
            |> List.mapi (fun lineNr lineContent -> {FileName=fc.FileName;LineNumber=lineNr + 1;LineContent=lineContent})
            |> List.where (fun result ->  
                result.LineContent |=| args.Pattern |> invertMod)
        )
        |> List.concat
        
let containFlag f flags = 
    flags &&& f = f
    
type MatchToString = GrepArgs -> MatchResult -> string
let matchToString : MatchToString = 

    fun args  match' ->
        let result = if containFlag FormatFlags.L args.FormatFlags then match'.FileName else match'.LineContent
        let lineNr = 
            if containFlag FormatFlags.N args.FormatFlags &&   
                not (containFlag FormatFlags.L args.FormatFlags)
            then match'.LineNumber.ToString() + ":" else ""
        
        let fileName = 
            if  args.FileNames.Length > 1 
                && not (containFlag FormatFlags.L args.FormatFlags)
                then match'.FileName + ":" 
                else ""

        sprintf "%s%s%s" fileName lineNr result


type Grep = GrepArgs -> string list //result

let grep'' : Grep =
    fun args ->
        FileContent.get () |> List.ofArray
        |> search args 
        |> List.map (matchToString args)
 
let _filterFlags= ["-v"; "-i"; "-x"] |> set
let _formatFlags= ["-l"; "-n";] |> set
let grep fileNmaes flags pattern    =  
    
    let flagSet =  set flags 

    let filterFlags = 
        flagSet - _formatFlags 
        |> Set.map (fun str -> match str.ToCharArray() with| [|hiphen;c2|] -> FilterFlags.from c2)
        |> Set.toList
        |> fun x -> List.fold  (|||)  (enum<FilterFlags> 0) x 
        
    let formatFlags = 
        flagSet - _filterFlags 
        |> Set.map (fun str -> match str.ToCharArray() with| [|hiphen;c2|] -> FormatFlags.from c2)
        |> Set.toList
        |> fun x -> List.fold  (|||)  (enum<FormatFlags> 0) x 
            

    grep'' {GrepArgs.FileNames = fileNmaes; Pattern= pattern; FormatFlags = formatFlags; FilterFlags =  filterFlags  }
    |> List.distinct
    
    