namespace FoolsTests

open Fools
open NUnit.Framework
open Swensen.Unquote.Assertions

[<TestFixture(typeof<InterpreterProvider>)>]
[<TestFixture(typeof<ReteInterpreterACProvider>)>]
type Tests<'IProvider when 'IProvider :> IInterpreterProvider
                      and 'IProvider : (new : unit -> 'IProvider)>() =
  let provider = new 'IProvider()
  let mkInterpreter rules =
    let convertRule (patterns, action) : AST.Rule = (List.map (fun s -> s,[]) patterns, AST.Insert(action,[]))
    provider.GetInterpreter <| List.map convertRule rules
  let getEmptyInterpreter() = mkInterpreter []

  let aFact = mkFact "A" []
  let bFact = mkFact "B" []

  let mkAImpliesB() = mkInterpreter [["A"],  "B"]

  [<Test>]
  member __.``a empty session has no facts``() =
    let i = getEmptyInterpreter()
    test <@ not <| i.HasFact aFact @>
    
  [<Test>]
  member __.``a fact can be added``() =
    let i = getEmptyInterpreter()
    i.Insert aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  member __.``a fact can be added twice``() =
    let i = getEmptyInterpreter()
    i.Insert aFact
    i.Insert aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  member __.``a non-added fact can be retracted``() =
    let i = getEmptyInterpreter()
    i.Retract aFact

  [<Test>]
  member __.``a fact can be added and removed``() =
    let i = getEmptyInterpreter()
    i.Insert aFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>

  [<Test>]
  member __.``rule with no assumptions triggers``() =
    let i = mkInterpreter [[], "A"]
    test <@ i.HasFact aFact @>

  [<Test>]
  member __.``consequences of rules with no assumptions cannot be removed``() =
    let i = mkInterpreter [[], "A"]
    i.Retract aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  member __.``consequences of rules with no assumptions are applied``() =
    let i = mkInterpreter [[], "A"
                           ["A"], "B"]
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``consequences of rules with no assumptions are applied to fixpoint cannot be removed``() =
    let i = mkInterpreter [[], "A"
                           ["A"], "B"]
    i.Retract aFact
    i.Retract bFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``rule with one assumption does not trigger, until assumption is fulfilled``() =
    let i = mkAImpliesB()
    test <@ not <| i.HasFact bFact @>

  [<Test>]
  member __.``rule with one assumption triggers when assumption is fulfilled``() =
    let i = mkAImpliesB()
    i.Insert aFact
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``a fact created by a rule cannot be removed``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Retract bFact
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``a fact confirmed by a rule cannot be removed``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Insert bFact
    i.Retract bFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``a fact confirmed by a rule will be removed when the rule deactivates``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Insert bFact
    i.Retract bFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>
    test <@ not <| i.HasFact bFact @>

  [<Test>]
  member __.``a fact is not retracted when it's no longer confirmed by the system``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Insert bFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``two rules can confirm the same fact``() =
    let i = mkInterpreter [["A"], "C";["B"], "C";]
    i.Insert aFact
    i.Insert bFact
    test <@ i.HasFact <| mkFact "C" [] @>

  [<Test>]
  member __.``two rules can confirm the same fact, and it is removed when the rules deactivate``() =
    let i = mkInterpreter [["A"], "C";["B"], "C";]
    i.Insert aFact
    i.Insert bFact
    i.Retract aFact
    i.Retract bFact
    test <@ not << i.HasFact <| mkFact "C" [] @>


  // circularity
  [<Test>]
  member __.``a rule can confirm its own assumption``() =
    let i = mkInterpreter [["A"], "A"]
    i.Insert aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  member __.``a fact can be removed if it's circularily confirmed by a rule``() =
    let i = mkInterpreter [["A"], "A"]
    i.Insert aFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>

  [<Test>]
  member __.``two facts can be created by two circular rules``() =
    let i = mkInterpreter [["A"], "B"
                           ["B"], "A"]
    i.Insert aFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  member __.``a fact can be removed if it's circularily confirmed by two rules``() =
    let i = mkInterpreter [["A"], "B"
                           ["B"], "A"]
    i.Insert aFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>
    test <@ not <| i.HasFact bFact @>

  [<Test>]
  member __.``a fact cannot be removed if it's circularily confirmed by two rules``() =
    let i = mkInterpreter [["A"], "B"
                           ["B"], "A"]
    i.Insert aFact
    i.Retract bFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>
