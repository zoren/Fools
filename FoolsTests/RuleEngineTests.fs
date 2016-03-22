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

  [<Test>]
  let ``when two facts are added do body is called twice`` () =
    let e, getState = mkEngine()
    let f1 = {Data = "hello"}
    e.AddFact f1
    let f2 = {Data = "hello2"}
    e.AddFact f2
    e.Fire()
    test <@ getState() = (2, 0) @>

  [<Test>]
  let ``when two facts are added the body triggers in the expected order``() =
    let e = ModelRuleEngine() :> IRuleEngine
    let activatedFacts = ref []
    let doBody =
      function
      | Fact f -> activatedFacts := (f :?> TestFact).Data :: !activatedFacts
    e.AddRule(FactOfType(typeof<TestFact>), (doBody, ignore))
    e.AddFact {Data = "a"}
    e.Fire()
    e.AddFact {Data = "b"}
    e.Fire()
    test <@ !activatedFacts = ["b"; "a"] @>
