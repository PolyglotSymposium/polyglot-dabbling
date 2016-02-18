open System.IO
open System.Text
open FSharp.Data

type private Parser = XmlProvider<"./inputexample.xml">

module Sort =
    let private by (selectBy : Parser.Row -> 'a) (table : Parser.Table) =
        Parser.Table(table.Name, table.Cols, Array.sortBy selectBy table.Rows)

    let bySequence =
        by (fun row -> row.Tds.[2].Number)

    let byAction =
        by (fun row -> row.Tds.[0].String)

let usage() =
    printfn "smfis - Save Me From InstallShield!!"
    printfn "Usage:"
    printfn "    smfis sequence <inputPath>"
    printfn "        Order the XML rows sequentially (as defined by InstallShield)"
    printfn "    smfis action <inputPath>"
    printfn "        Order the XML rows alphabetically by action (as defined by InstallShield)"
    printfn "    smfis help"
    printfn "        Get this help message"

let exec (sort : Parser.Table -> Parser.Table) inputPath =
    let xml = File.ReadAllText(inputPath, Encoding.UTF8)
    let xml' = sort <| Parser.Parse xml
    System.Console.Write(xml')

[<EntryPoint>]
let main argv = 
    match argv with
    | [|"help"|] -> 
        usage()
        0
    | [|"sequence"; inputPath|] ->
        exec Sort.bySequence inputPath
        0
    | [|"action"; inputPath|] ->
        exec Sort.byAction inputPath
        0
    | _ ->
        printfn "Error: not clear what you want. Please see below."
        usage()
        1
