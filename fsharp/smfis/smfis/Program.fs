open System.IO
open System.Text
open System.Xml.Linq
open FSharp.Data

type private Parser = XmlProvider<"./inputexample.xml">

module Sort =
    let private by (selectBy : Parser.Row -> 'a) (table : Parser.Table) =
        let sorted = Array.sortBy selectBy table.Rows
        let table = XElement(XName.Get "table", seq {
            yield table.XElement.FirstAttribute :> XObject
            for col in table.Cols do yield col.XElement :> XObject
            for row in sorted do yield row.XElement :> XObject
        })
        let doc = XDocument(table)
        doc.Declaration <- XDeclaration("1.0", "utf-8", "no")
        doc

    let bySequence =
        by (fun row -> row.Tds.[2].Number)

    let byAction =
        by (fun row -> row.Tds.[0].String)

let usage() =
    printfn "smfis - Save Me From InstallShield!!"
    printfn "Usage:"
    printfn "    smfis sequence <inputPath> <outputPath>"
    printfn "        Order the XML rows sequentially (as defined by InstallShield)"
    printfn "    smfis action <inputPath> <outputPath>"
    printfn "        Order the XML rows alphabetically by action (as defined by InstallShield)"
    printfn "    smfis help"
    printfn "        Get this help message"

let exec (sort : Parser.Table -> XDocument) inputPath (outputPath : string) =
    let xml = File.ReadAllText(inputPath, Encoding.UTF8)
    let parsedXml = Parser.Parse xml
    let sortedXml = sort parsedXml
    sortedXml.Save(outputPath)

[<EntryPoint>]
let main argv = 
    match argv with
    | [|"help"|] -> 
        usage()
        0
    | [|"sequence"; inputPath; outputPath|] ->
        exec Sort.bySequence inputPath outputPath
        0
    | [|"action"; inputPath; outputPath|] ->
        exec Sort.byAction inputPath outputPath
        0
    | _ ->
        printfn "Error: not clear what you want. Please see below."
        usage()
        1
