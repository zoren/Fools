namespace FoolsTests

open Fools
open NUnit.Framework
open Swensen.Unquote.Assertions

[<TestFixture>]
module Tests =
  let mkInterpreter rules = Fools.Interpreter rules
  let getEmptyInterpreter() = mkInterpreter []

  let aFact = mkFact "A" []
  let bFact = mkFact "B" []

  [<Test>]
  let ``a empty session has no facts``() =
    let i = getEmptyInterpreter()
    test <@ not <| i.HasFact aFact @>
    
  [<Test>]
  let ``a fact can be added``() =
    let i = getEmptyInterpreter()
    i.Insert aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  let ``a fact can be added twice``() =
    let i = getEmptyInterpreter()
    i.Insert aFact
    i.Insert aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  let ``a non-added fact can be retracted``() =
    let i = getEmptyInterpreter()
    i.Retract aFact

  [<Test>]
  let ``a fact can be added and removed``() =
    let i = getEmptyInterpreter()
    i.Insert aFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>

  [<Test>]
  let ``rule with no assumptions triggers``() =
    let i = mkInterpreter [[], AST.Insert "A"]
    test <@ i.HasFact aFact @>

  [<Test>]
  let ``consequences of rules with no assumptions cannot be removed``() =
    let i = mkInterpreter [[], AST.Insert "A"]
    i.Retract aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  let ``consequences of rules with no assumptions are applied``() =
    let i = mkInterpreter [[], AST.Insert "A"
                           ["A"], AST.Insert "B"]
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  let ``consequences of rules with no assumptions are applied to fixpoint cannot be removed``() =
    let i = mkInterpreter [[], AST.Insert "A"
                           ["A"], AST.Insert "B"]
    i.Retract aFact
    i.Retract bFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  let mkAImpliesB() = mkInterpreter [["A"], AST.Insert "B"]

  [<Test>]
  let ``rule with one assumption does not trigger, until assumption is fulfilled``() =
    let i = mkAImpliesB()
    test <@ not <| i.HasFact bFact @>

  [<Test>]
  let ``rule with one assumption triggers when assumption is fulfilled``() =
    let i = mkAImpliesB()
    i.Insert aFact
    test <@ i.HasFact bFact @>

  [<Test>]
  let ``a fact created by a rule cannot be removed``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Retract bFact
    test <@ i.HasFact bFact @>

  [<Test>]
  let ``a fact confirmed by a rule cannot be removed``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Insert bFact
    i.Retract bFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  let ``a fact confirmed by a rule will be removed when the rule deactivates``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Insert bFact
    i.Retract bFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>
    test <@ not <| i.HasFact bFact @>

  [<Test>]
  let ``a fact is not retracted when it's no longer confirmed by the system``() =
    let i = mkAImpliesB()
    i.Insert aFact
    i.Insert bFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  // circularity
  [<Test>]
  let ``a rule can confirm its own assumption``() =
    let i = mkInterpreter [["A"], AST.Insert "A"]
    i.Insert aFact
    test <@ i.HasFact aFact @>

  [<Test>]
  let ``a fact can be removed if it's circularily confirmed by a rule``() =
    let i = mkInterpreter [["A"], AST.Insert "A"]
    i.Insert aFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>

  [<Test>]
  let ``two facts can be created by two circular rules``() =
    let i = mkInterpreter [["A"], AST.Insert "B"
                           ["B"], AST.Insert "A"]
    i.Insert aFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>

  [<Test>]
  let ``a fact can be removed if it's circularily confirmed by two rules``() =
    let i = mkInterpreter [["A"], AST.Insert "B"
                           ["B"], AST.Insert "A"]
    i.Insert aFact
    i.Retract aFact
    test <@ not <| i.HasFact aFact @>
    test <@ not <| i.HasFact bFact @>

  [<Test>]
  let ``a fact cannot be removed if it's circularily confirmed by two rules``() =
    let i = mkInterpreter [["A"], AST.Insert "B"
                           ["B"], AST.Insert "A"]
    i.Insert aFact
    i.Retract bFact
    test <@ i.HasFact aFact @>
    test <@ i.HasFact bFact @>
