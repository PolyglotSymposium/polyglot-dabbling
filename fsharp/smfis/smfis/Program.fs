namespace smfis

module EntryPoint =
    open System.IO
    open System.Text
    open System.Xml.Linq

    let private usage() =
        printfn "smfis - Save Me From InstallShield!!"
        printfn "Usage:"
        printfn "    smfis sort --by-sequence <inputPath> [<outputPath>]"
        printfn "        Order the XML rows sequentially (as defined by InstallShield)"
        printfn "    smfis sort --by-action <inputPath> [<outputPath>]"
        printfn "        Order the XML rows alphabetically by action (as defined by InstallShield)"
        printfn "    smfis repair-seq <inputPath> [<outputPath>]"
        printfn "        Run through the sequence numbers, incrementing where necessary to enforce proper sequencing"
        printfn "    smfis help"
        printfn "        Get this help message"

    let private exec' (action : Parser.Table -> XDocument) inputPath =
        let xml = File.ReadAllText(inputPath, Encoding.UTF8)
        let parsedXml = Parser.Parse xml
        action parsedXml

    let private saveTo (outputPath : string) (outputXml : XDocument) =
        outputXml.Save outputPath

    let private printXml (outputXml : XDocument) =
        outputXml.Save stdout

    let private exec (action : Parser.Table -> XDocument) paths =
        match paths with
        | [inputPath] ->
            exec' action inputPath
            |> printXml
            0
        | [inputPath; outputPath] ->
            exec' action inputPath
            |> saveTo outputPath
            0
        | _ ->
            stderr.WriteLine (sprintf "Error: wrong number of paths: %i" paths.Length)
            stderr.WriteLine "Expected an input path and optionally an output path"
            1

    [<EntryPoint>]
    let main argv = 
        match List.ofArray argv with
        | ["help"] -> 
            usage()
            0
        | "repair-seq" :: paths ->
            exec Repair.sequencing paths
        | "sort" :: by :: paths ->
            match by with
            | "--by-sequence" -> 
                exec (Sort.by InputXml.sequence) paths
            | "--by-action" ->
                exec (Sort.by InputXml.action) paths
            | _ ->
                stderr.WriteLine "Error: unrecognized sort option"
                stderr.WriteLine "Valid sort options are --by-sequence and --by-action"
                2
        | _ ->
            stderr.WriteLine "Error: not clear what you want. Please see below."
            usage()
            3
