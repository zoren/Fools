namespace Fools

[<AutoOpen>]
module FactHelper =
  type Fact = AST.FactName
  let mkFact (factName:AST.FactName) args =
    if args <> []
    then failwith "args not yet supported"
    else factName : Fact

type IInterpreter =
  abstract member HasFact : Fact -> bool
  abstract member Insert : Fact -> unit
  abstract member Retract : Fact -> unit

type IInterpreterProvider =
  abstract member GetInterpreter : AST.File -> IInterpreter