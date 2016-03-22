namespace Fools

type Fact = System.Object
type FactType = System.Type

type Ast =
  | FactOfType of FactType

type Just =
  | Fact of Fact

type RuleBody = Just -> unit

type IRuleEngine =
  abstract member RegisterFactType : FactType -> unit
  abstract member AddRule : Ast * RuleBody -> unit
  abstract member AddFact : Fact -> unit
  abstract member Fire : unit -> unit

open System.Collections.Generic

type RuleEngine() =
  interface IRuleEngine with
    member __.RegisterFactType factType = ()

    member __.AddRule(cond, body) = ()

    member __.AddFact fact = ()

    member __.Fire () = ()
