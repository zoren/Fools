namespace Fools.Test

open Fools
open System.Collections.Generic

[<AutoOpen>]
module Helpers =
  [<CustomEquality; CustomComparison>]
  type Box =
    { Object : obj }
    override this.GetHashCode() = this.Object.GetHashCode()
    override x.Equals yObj =
      match yObj with
      | :? Box as y -> x = y
      | _ -> invalidArg "yObj" "expected Box"
    interface System.IComparable with
     member x.CompareTo y = compare (x.GetHashCode()) (y.GetHashCode())

  let mkBox o = {Object = o}

  type InternalJustification =
    | Fact of Box

  let internalToExternal =
    function
    | Fact b -> Justification.Fact b.Object

type ModelRuleEngine() =
  let rules = List<_>()

  let factSet = Dictionary<Fact, Box>()

  let mutable activations = Set.empty

  let patternToPicker : Pattern -> Box seq -> Set<InternalJustification> =
    function
    | FactOfType ft ->
      fun (bs:Box seq) ->
        bs |> Seq.choose (fun b ->
                            if b.Object.GetType() = ft
                            then Some <| Fact b
                            else None)
           |> set

  let getActivations (facts: Box seq) rules =
    let compRules =
      rules |> Seq.map (fun(pat, body) -> patternToPicker pat, body) |> Seq.cache
    compRules |> Seq.collect (fun (cPat, body) -> cPat facts |> Set.map (fun j -> j, body)) |> set

  interface IRuleEngine with
    member __.AddRule(rule) =
      rules.Add rule

    member __.AddFact fact =
      ignore <| factSet.Add(fact, mkBox fact)

    member __.RemoveFact fact =
      ignore <| factSet.Remove fact

    member __.Fire () =
      let numberedRules = rules |> Seq.mapi (fun i (pattern, _) -> pattern, i)
      let newActivations = getActivations factSet.Values numberedRules
      let oldActivations = activations
      activations <- newActivations
      let addedActivations = Set.difference newActivations oldActivations
      let removedActivations = Set.difference oldActivations newActivations
      addedActivations
        |> Seq.sortBy snd
        |> Seq.iter (fun (intJust, ruleIndex) ->
                          let extJust = internalToExternal intJust
                          let doBody = rules.[ruleIndex] |> snd |> fst
                          doBody extJust)
      removedActivations
        |> Seq.sortBy snd
        |> Seq.iter (fun (intJust, ruleIndex) ->
                          let extJust = internalToExternal intJust
                          let undoBody = rules.[ruleIndex] |> snd |> snd
                          undoBody extJust)
