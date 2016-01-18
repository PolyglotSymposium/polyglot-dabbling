[<ReflectedDefinition>]
module Program

open FunScript
open FSharp.Data
open System.IO

// Open up the world bank type provider
type WorldBank = WorldBankDataProvider<Asynchronous=true>

// Create some operators for JQuery interop that make
// use of the library mappings.  Allows writing 'jq?name' for element access
let jq(selector : string) = Globals.Dollar.Invoke selector
let (?) jq name = jq("#" + name)

// Create a function that will be compiled into JavaScript...
let main () =
    let data = WorldBank.GetDataContext()

    // Pull out some countries from the WorldBank...
    let countries = [|
        data.Countries.``United Kingdom``
        data.Countries.``United States``
        data.Countries.France
        data.Countries.Italy
        data.Countries.``Russian Federation``
        data.Countries.Norway
        data.Countries.China
    |]

    // For each of the countries selected...
    countries |> Array.iter (fun c ->
        async {
            // Asynchronously pull out the percentage of income
            // held by the highest 10% of earners...
            let! indicator = c.Indicators.``Income share held by highest 10%``
            let incomeShare = indicator |> Seq.last |> snd
            // Append some text to the "results" div...
            jq?results.append(jq ("<p>" + c.Name + " : " + incomeShare.ToString() + "%</p>")) |> ignore
        } |> Async.StartImmediate
    )

// Compile the main() function into javascript code...
let code =
    Compiler.Compiler.Compile(
        // This argument is the quotation to compile
        <@ main() @>, 
        // This argument tells the compiler not to wrap 
        // the result in a return statement
        noReturn=true, 
        // This tells the compiler about the additional components 
        // that are needed to compile the code. In this case,
        // these are the components that provide mappings for the
        // FSharp.Data type providers (incl. the WorldBank provider).
        components = FunScript.Data.Components.DataProviders  )

// Here we define the page we'll create...
// Note: You have to manually include references to the JavaScript dependencies.
let page = sprintf """<!DOCTYPE html>
<html>
    <head>
        <title>Sample</title>
        <script src="http://code.jquery.com/jquery-1.8.0.js"></script>
    </head>
    <body>
        <div id="results">
            <p><strong>Income share held by top 10%%:</strong></p>
        </div>

        <script>
%s
        </script>
    </body>
</html>"""          code

// We write the page to a file...
let filePath = Path.Combine(System.Environment.CurrentDirectory, "index.html")
File.WriteAllText(filePath, page)
// We open the file in the default web browser...
System.Diagnostics.Process.Start(filePath) |> ignore