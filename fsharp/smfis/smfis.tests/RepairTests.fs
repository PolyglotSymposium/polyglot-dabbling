namespace smfis.tests

open System.IO
open System.Text
open System.Xml.Linq
open FsUnit
open NUnit.Framework
open smfis

[<TestFixture>]
type ``Repairing sequencing``() = 

    [<Test>]
    member self.``should work as expected on pre-sorted sample XML``() =
        let sample = """
<table name="InstallExecuteSequence">
    <col key="yes" def="s72">Action</col>
    <col def="S255">Condition</col>
    <col def="I2">Sequence</col>
    <col def="S255">ISComments</col>
    <col def="I4">ISAttributes</col>
    <row><td>DLLWrapStartup</td><td/><td>4</td><td/><td/></row>
    <row><td>AppSearch</td><td/><td>426</td><td>AppSearch</td><td/></row>
    <row><td>ConfigureAppSearchResults</td><td/><td>426</td><td/><td/></row>
    <row><td>CCPSearch</td><td>CCP_TEST</td><td>427</td><td>CCPSearch</td><td/></row>
    <row><td>CostInitialize</td><td/><td>426</td><td>CostInitialize</td><td/></row>
    <row><td>CostFinalize</td><td/><td>1000</td><td>CostFinalize</td><td/></row>
    <row><td>AllocateRegistrySpace</td><td>NOT Installed</td><td>1550</td><td>AllocateRegistrySpace</td><td/></row>
    <row><td>CreateFolders</td><td/><td>3700</td><td>CreateFolders</td><td/></row>
    <row><td>ConfigureDbPaths</td><td/><td>4216</td><td/><td/></row>
    <row><td>CreateFolders_Local</td><td/><td>4216</td><td/><td/></row>
    <row><td>BindImage</td><td/><td>5189</td><td>BindImage</td><td/></row>
    <row><td>CreateShortcuts</td><td/><td>5189</td><td>CreateShortcuts</td><td/></row>
    <row><td>DLLWrapCleanup</td><td/><td>6601</td><td/><td/></row>
</table>"""

        let expected = """<?xml version="1.0" encoding="utf-8" standalone="no"?><table name="InstallExecuteSequence"><col key="yes" def="s72">Action</col><col def="S255">Condition</col><col def="I2">Sequence</col><col def="S255">ISComments</col><col def="I4">ISAttributes</col><row><td>DLLWrapStartup</td><td /><td>4</td><td /><td /></row><row><td>AppSearch</td><td /><td>426</td><td>AppSearch</td><td /></row><row><td>ConfigureAppSearchResults</td><td /><td>427</td><td /><td /></row><row><td>CCPSearch</td><td>CCP_TEST</td><td>428</td><td>CCPSearch</td><td /></row><row><td>CostInitialize</td><td /><td>429</td><td>CostInitialize</td><td /></row><row><td>CostFinalize</td><td /><td>1000</td><td>CostFinalize</td><td /></row><row><td>AllocateRegistrySpace</td><td>NOT Installed</td><td>1550</td><td>AllocateRegistrySpace</td><td /></row><row><td>CreateFolders</td><td /><td>3700</td><td>CreateFolders</td><td /></row><row><td>ConfigureDbPaths</td><td /><td>4216</td><td /><td /></row><row><td>CreateFolders_Local</td><td /><td>4217</td><td /><td /></row><row><td>BindImage</td><td /><td>5189</td><td>BindImage</td><td /></row><row><td>CreateShortcuts</td><td /><td>5190</td><td>CreateShortcuts</td><td /></row><row><td>DLLWrapCleanup</td><td /><td>6601</td><td /><td /></row></table>"""
        
        let repaired = Parser.Parse sample
                       |> Repair.sequencing
        let memStream = new MemoryStream()
        repaired.Save(memStream, SaveOptions.DisableFormatting)
        let actual = Encoding.UTF8.GetString <| memStream.ToArray()

        actual.[1..] |> should equal expected
