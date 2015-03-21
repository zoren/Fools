namespace FoolsTests

open Fools
open NUnit.Framework
open Swensen.Unquote.Assertions

[<TestFixture>]
module Tests =
  let getEmptyInterpreter() = Fools.Interpreter []
    
  [<Test>]
  let ``a fact can be added``() =
    let i = getEmptyInterpreter()
    let fact = mkFact "A" []    
    i.Insert fact
    test <@ i.HasFact fact @>

  [<Test>]
  let ``reading fact causes exception``() =
    let i = getEmptyInterpreter()
    let fact = mkFact "A" []    
    i.Insert fact
    raises<System.ArgumentException>  <@ i.Insert fact @>

  [<Test>]
  let ``a fact can be added and removed``() =
    let i = getEmptyInterpreter()
    let fact = mkFact "A" []    
    i.Insert fact
    i.Retract fact
    test <@ not <| i.HasFact fact @>

  [<Test>]
  let ``retracting non-inserted fact causes exception``() =
    let i = getEmptyInterpreter()
    let fact = mkFact "A" []    
    raises<System.ArgumentException> <@ i.Retract fact @>
