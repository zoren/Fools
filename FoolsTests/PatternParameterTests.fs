namespace FoolsTests

open Fools
open AST
open NUnit.Framework
open Swensen.Unquote.Assertions

[<TestFixture(typeof<InterpreterProvider>)>]
[<TestFixture(typeof<ReteInterpreterACProvider>)>]
[<TestFixture(typeof<Incremental.IncrementalProvider>)>]
type PatternParameterTests<'IProvider when 'IProvider :> IInterpreterProvider
                                      and 'IProvider : (new : unit -> 'IProvider)>() =
  let provider = new 'IProvider()
  let mkInterpreter rules = provider.GetInterpreter rules

  [<Test>]
  member __.``a constants are matched``() =
    let i = mkInterpreter [["A",[PatConst "HEJ"]], AST.Insert ("B",[])]
    i.Insert <| mkFact "A" ["HEJ"]
    test <@ i.HasFact <| mkFact "B" [] @>

  [<Test>]
  member __.``a variable can be bound``() =
    let i = mkInterpreter [["A",[PatVar "x"]], AST.Insert ("B",[])]
    i.Insert <| mkFact "A" ["HEJ"]
    test <@ i.HasFact <| mkFact "B" [] @>

  [<Test>]
  member __.``a variable can be bound in inserts``() =
    let i = mkInterpreter [["A",[PatVar "x"]], AST.Insert ("B", [Var "x"])]
    i.Insert <| mkFact "A" ["HEJ"]
    test <@ i.HasFact <| mkFact "B" ["HEJ"] @>

  [<Test>]
  member __.``a variable can be bound in inserts, building a product``() =
    let i = mkInterpreter [["A",[PatVar "x"];"B",[PatVar "y"]], AST.Insert ("C", [Var "x"; Var "y"])]
    i.Insert <| mkFact "A" ["1"]
    i.Insert <| mkFact "A" ["2"]
    i.Insert <| mkFact "B" ["10"]
    test <@ i.HasFact <| mkFact "C" ["1";"10"] @>
    test <@ i.HasFact <| mkFact "C" ["2";"10"] @>

  [<Test>]
  member __.``a variable can be bound in inserts, building a products``() =
    let i = mkInterpreter [["A",[PatVar "x"];"B",[PatVar "y"]], AST.Insert ("C", [Var "x"; Var "y"])]
    i.Insert <| mkFact "A" ["1"]
    i.Insert <| mkFact "A" ["2"]
    i.Insert <| mkFact "B" ["10"]
    i.Insert <| mkFact "B" ["20"]
    test <@ i.HasFact <| mkFact "C" ["1";"10"] @>
    test <@ i.HasFact <| mkFact "C" ["2";"10"] @>
    test <@ i.HasFact <| mkFact "C" ["1";"20"] @>
    test <@ i.HasFact <| mkFact "C" ["2";"20"] @>

  [<Test>]
  member __.``a variable can be bound twice in a pattern``() =
    let i = mkInterpreter [["A",[PatVar "x"; PatVar "x"]], AST.Insert ("B", [Var "x"])]
    i.Insert <| mkFact "A" ["1"; "2"]
    test <@ not << i.HasFact <| mkFact "B" ["1"] @>
    test <@ not << i.HasFact <| mkFact "B" ["2"] @>
    i.Insert <| mkFact "A" ["1"; "1"]
    test <@ i.HasFact <| mkFact "B" ["1"] @>

  [<Test>]
  member __.``a variable can be bound two different patterns``() =
    let i = mkInterpreter [["A",[PatVar "x"];"B",[PatVar "x"]], AST.Insert ("C", [Var "x"])]
    i.Insert <| mkFact "A" ["1"]
    i.Insert <| mkFact "B" ["2"]
    test <@ not << i.HasFact <| mkFact "C" ["1"] @>
    test <@ not << i.HasFact <| mkFact "C" ["2"] @>
    i.Insert <| mkFact "B" ["1"]
    test <@ i.HasFact <| mkFact "C" ["1"] @>
