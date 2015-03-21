namespace Fools

[<AutoOpen>]
module FactHelper =
  type Fact = AST.FactName * AST.Constant list
  let mkFact (factName:AST.FactName) (args : AST.Constant list) = factName, args : Fact

type IInterpreter =
  abstract member HasFact : Fact -> bool
  abstract member Insert : Fact -> unit
  abstract member Retract : Fact -> unit

type IInterpreterProvider =
  abstract member GetInterpreter : AST.File -> IInterpreter