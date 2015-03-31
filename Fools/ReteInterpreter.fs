namespace Fools

open AST

type ReteInterpreter(file:AST.File) =
  let graph = ReteHelper.fileToGraph file

  let mutable userSet = Set.empty

  interface IInterpreter with
    member __.HasFact fact =
      let systemSet = ReteHelper.evalRulesToFix graph userSet
      Set.contains fact systemSet

    member __.Insert fact =
      userSet <- Set.add fact userSet

    member __.Retract fact =
      userSet <- Set.remove fact userSet

type public ReteInterpreterProvider() =
  interface IInterpreterProvider with
    member __.GetInterpreter file = ReteInterpreter file :> IInterpreter
