namespace Fools.Test

open NUnit.Framework

[<TestFixture>]
module RuleEngineTests =
  open Fools
  open Swensen.Unquote.Assertions

  type TestFact = {Data : string}

  let mkEngine() =
    let e = ModelRuleEngine() :> IRuleEngine
    let doCountRef, undoCountRef = ref 0, ref 0
    e.AddRule(FactOfType(typeof<TestFact>), ((fun _ -> incr doCountRef), (fun _ -> incr undoCountRef)))
    e, fun() -> !doCountRef, !undoCountRef

  [<Test>]
  let ``when a fact is added do body is called`` () =
    let e, getState = mkEngine()
    let f1 = {Data = "hello"}
    e.AddFact f1
    e.Fire()
    test <@ getState() = (1, 0) @>

  [<Test>]
  let ``when a fact is removed undo body is called`` () =
    let e, getState = mkEngine()
    let f1 = {Data = "hello"}
    e.AddFact f1
    e.Fire()
    e.RemoveFact f1
    e.Fire()
    test <@ getState() = (1, 1) @>
