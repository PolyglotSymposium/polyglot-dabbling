namespace smfis

[<AutoOpen>]
module ``you put it in just so you can take it out`` =
    open FSharp.Data

    type Parser = XmlProvider<"""<table name="InstallExecuteSequence">
		<col key="yes" def="s72">Action</col>
		<col def="S255">Condition</col>
		<col def="I2">Sequence</col>
		<col def="S255">ISComments</col>
		<col def="I4">ISAttributes</col>
		<row><td>AllocateRegistrySpace</td><td>NOT Installed</td><td>1550</td><td>AllocateRegistrySpace</td><td/></row>
		<row><td>AppSearch</td><td/><td>426</td><td>AppSearch</td><td/></row>
		<row><td>BindImage</td><td/><td>5189</td><td>BindImage</td><td/></row>
		<row><td>CCPSearch</td><td>CCP_TEST</td><td>500</td><td>CCPSearch</td><td/></row>
		<row><td>ConfigureAppSearchResults</td><td/><td>428</td><td/><td/></row>
		<row><td>ConfigureDbPaths</td><td/><td>4216</td><td/><td/></row>
		<row><td>CostFinalize</td><td/><td>1000</td><td>CostFinalize</td><td/></row>
		<row><td>CostInitialize</td><td/><td>800</td><td>CostInitialize</td><td/></row>
		<row><td>CreateFolders</td><td/><td>3700</td><td>CreateFolders</td><td/></row>
		<row><td>CreateFolders_Local</td><td/><td>4219</td><td/><td/></row>
		<row><td>CreateShortcuts</td><td/><td>5190</td><td>CreateShortcuts</td><td/></row>
		<row><td>DLLWrapCleanup</td><td/><td>6601</td><td/><td/></row>
		<row><td>DLLWrapStartup</td><td/><td>4</td><td/><td/></row>
	</table>""">

module InputXml =
    let sequence (row : Parser.Row) =
        row.Tds.[2].Number

    let action (row : Parser.Row) =
        row.Tds.[0].String

module OutputXml =
    open System.Xml.Linq

    let tableDocument (table : Parser.Table) (rows : XObject seq) =
        let table = XElement(XName.Get "table", seq {
            yield table.XElement.FirstAttribute :> XObject
            for col in table.Cols do yield col.XElement :> XObject
            yield! rows
        })
        let doc = XDocument(table)
        doc.Declaration <- XDeclaration("1.0", "utf-8", "no")
        doc