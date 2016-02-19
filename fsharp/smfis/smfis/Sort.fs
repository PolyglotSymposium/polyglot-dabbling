namespace smfis

module Sort =
    open System.Xml.Linq

    let by (selectBy : Parser.Row -> 'a) (table : Parser.Table) =
        Seq.sortBy selectBy table.Rows
        |> Seq.map (fun row -> row.XElement :> XObject)
        |> OutputXml.tableDocument table
