module Runner

open Expecto

[<EntryPoint>]
let main args =
    let config = { defaultConfig with ``parallel`` = true }
    runTestsWithArgs config args Tests.tests
