namespace Fools

[<AutoOpen>]
module FactHelper =
  type Fact = AST.FactName
  let mkFact (factName:AST.FactName) args =
    if args <> []
    then failwith "args not yet supported"
    else factName : Fact

type Interpreter(file:AST.File) =
  let mutable factSet = Set.empty

  member __.HasFact (fact:Fact) : bool =
    Set.contains fact factSet

  member __.Insert (fact:Fact) : unit =
    factSet <- Set.add fact factSet

  member __.Retract (fact:Fact) : unit =
    factSet <- Set.remove fact factSet
