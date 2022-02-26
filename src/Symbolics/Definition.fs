namespace MathNet.Symbolics

module Definition =

    let funDict = new System.Collections.Concurrent.ConcurrentDictionary<string, (Symbol list) * Expression>()

    let define fnm exp =
        funDict.AddOrUpdate(
            fnm
            , (fun nm -> exp)
            , (fun nm cur_exp -> exp)
        )
