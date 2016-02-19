namespace smfis

module Repair =
    open System.Xml.Linq

    let private repairSeqNum (prevSeqNum : int, prevRowXml) (row : Parser.Row) =
        let repairSeqNum' _ seqNum =
            if seqNum > prevSeqNum
            then seqNum, row.XElement
            else
                let seqNum' = prevSeqNum + 1
                in seqNum', (XElement(XName.Get "row", seq {
                    yield row.Tds.[0].XElement
                    yield row.Tds.[1].XElement
                    yield XElement(XName.Get "td", seqNum')
                    yield row.Tds.[3].XElement
                    yield row.Tds.[4].XElement
                }))
        Option.fold repairSeqNum' (prevSeqNum, row.XElement) (InputXml.sequence row)

    let sequencing (table : Parser.Table) =
        let initial = repairSeqNum (0, XElement(XName.Get "row")) (Seq.head table.Rows)
        table.Rows.[1..]
        |> Seq.scan repairSeqNum initial
        |> Seq.map (fun x -> snd x :> XObject)
        |> OutputXml.tableDocument table

