﻿namespace Fools

type Fact = System.Object
type FactType = System.Type

type Pattern =
  | FactOfType of FactType

type Justification =
  | Fact of Fact

type RuleBody = (Justification -> unit) * (Justification -> unit)

type Rule = Pattern * RuleBody

type IRuleEngine =
  abstract member AddRule : Rule -> unit
  abstract member AddFact : Fact -> unit
  abstract member RemoveFact : Fact -> unit
  abstract member Fire : unit -> unit

[<AutoOpen>]
module RuleEngineHelpers =
  let getFact<'a> =
    function
    | Fact f -> f :?> 'a