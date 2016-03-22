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

  type InternalJustification =
    | Fact of Box

  let internalToExternal =
    function
    | Fact b -> Justification.Fact b.Object

type ModelRuleEngine() =
  let rules = List<_>()

  let factSet = HashSet<_>()

  let mutable activations = Set.empty

  let patternToPicker : Pattern -> Fact seq -> Set<InternalJustification> =
    function
    | FactOfType ft ->
      fun (fs:Fact seq) ->
        fs |> Seq.choose (fun f ->
                            if f.GetType() = ft
                            then Some <| Fact { Object = f }
                            else None)
           |> set

  let getActivations (facts: Fact seq) rules =
    let compRules =
      rules |> Seq.map (fun(pat, body) -> patternToPicker pat, body) |> Seq.cache
    compRules |> Seq.map (fun (cPat, body) -> cPat facts, body) |> set

  interface IRuleEngine with
    member __.AddRule(rule) =
      rules.Add rule

    member __.AddFact fact =
      ignore <| factSet.Add fact

    member __.RemoveFact fact =
      ignore <| factSet.Remove fact

    member __.Fire () =
      let numberedRules = rules |> Seq.mapi (fun i (pattern, _) -> pattern, i)
      let newActivations = getActivations factSet numberedRules
      let oldActivations = activations
      activations <- newActivations
      let addedActivations = Set.difference newActivations oldActivations
      let removedActivations = Set.difference oldActivations newActivations
      addedActivations
        |> Seq.sortBy snd
        |> Seq.iter (fun (intJusts, ruleIndex) ->
                          intJusts
                            |> Seq.iter (fun intJust ->
                                          let extJust = internalToExternal intJust
                                          let doBody = rules.[ruleIndex] |> snd |> fst
                                          doBody extJust))
      removedActivations
        |> Seq.sortBy snd
        |> Seq.iter (fun (intJusts, ruleIndex) ->
                          intJusts
                            |> Seq.iter (fun intJust ->
                                          let extJust = internalToExternal intJust
                                          let undoBody = rules.[ruleIndex] |> snd |> snd
                                          undoBody extJust))

