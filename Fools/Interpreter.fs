namespace Fools

[<AutoOpen>]
module FactHelper =
  type Fact = AST.FactName
  let mkFact (factName:AST.FactName) args =
    if args <> []
    then failwith "args not yet supported"
    else factName : Fact

type Interpreter(file:AST.File) =
  member __.HasFact (fact:Fact) : bool =
    failwith "not implemented"

  member __.Insert (fact:Fact) : unit =
    failwith "not implemented"

  member __.Retract (fact:Fact) : unit =
    failwith "not implemented"
