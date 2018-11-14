module Runner

open Expecto

[<EntryPoint>]
let main args =
    let writeResults = TestResults.writeNUnitSummary ("TestResults.xml", "Expecto.Tests")
    let config = defaultConfig.appendSummaryHandler writeResults
    runTestsWithArgs config args Tests.tests
