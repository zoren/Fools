namespace Fools

open AST

module ReteAlphaConverted =
  type RetePatternParam = ReteAnything | RetePatConst of Constant
  type ReteFactPattern = FactName * RetePatternParam list
  type TokenSelector =
    | Left of TokenSelector
    | Right of TokenSelector
    | FactIndex of int
  type ConstraintTest = TokenSelector * TokenSelector
  type NodeAC =
    | Dummy
    | Alpha of ReteFactPattern
    | Join of NodeAC * NodeAC
    | Constraint of NodeAC * ConstraintTest

  let patParamToRete =
    function
    | PatVar _ -> ReteAnything
    | PatConst c -> RetePatConst c

  let merge (m, eqSet) (var, sel) =
    Map.add var sel m,
      match Map.tryFind var m with
      | None -> eqSet
      | Some sel' -> Set.add (if sel < sel' then sel, sel' else sel', sel) eqSet

  let factPatternToAlpha (patternParams:PatternParameter list) =
    let toMap i =
      function
      | PatVar v -> Some(v, FactIndex i)
      | PatConst _ -> None
    Seq.mapi toMap patternParams
      |> Seq.choose id
      |> Seq.fold merge (Map.empty, Set.empty)

  let rec alphaConvert =
    function
    | ReteHelper.Dummy -> NodeAC.Dummy, Map.empty
    | ReteHelper.Alpha(factName, patternParams) ->
      let map, eqSet = factPatternToAlpha patternParams
      let alphaNode = NodeAC.Alpha(factName, List.map patParamToRete patternParams)
      Seq.fold (fun n (l, r) -> NodeAC.Constraint(n, (l, r))) alphaNode <| Set.toSeq eqSet, map
    | ReteHelper.Join(l, r) ->
      let nl, ml = alphaConvert l
      let nr, mr = alphaConvert r
      let m' =
        Seq.append
          (Map.toSeq ml |> Seq.map (fun (v, i) -> v, Left i))
          (Map.toSeq mr |> Seq.map (fun (v, i) -> v, Right i)) |> Map.ofSeq
      let eqList = Map.toArray ml |> Seq.choose (fun(vl, il) -> Option.map (fun ir -> il, ir) <| Map.tryFind vl mr) |> Set.ofSeq
      NodeAC.Join(nl, nr), m'

  type Token =
    | UnitToken
    | FactToken of Fact
    | JoinToken of Token * Token

  let matchPatternParam c =
    function
    | ReteAnything -> true
    | RetePatConst c' -> c = c'

  let matchConstants consts reteFactPattern =
    if List.length reteFactPattern <> List.length consts
    then failwith "arity mismatch"
    List.forall2 matchPatternParam consts reteFactPattern

  let matchPattern (factName, reteFactPattern) ((factName', consts)) =
    factName = factName' && matchConstants consts reteFactPattern

  let rec lookup token selector =
    match token, selector with
    | FactToken (_, fact), FactIndex i -> List.nth fact i
    | JoinToken(l, _), Left sel -> lookup l sel
    | JoinToken(_, r), Right sel -> lookup r sel
    | _ -> failwith "not found"

  let evalNode factSet =
    let rec loop =
      function
      | NodeAC.Dummy -> Seq.singleton UnitToken
      | NodeAC.Alpha reteFactPattern ->
        factSet
          |> Seq.filter (matchPattern reteFactPattern)
          |> Seq.map FactToken
      | NodeAC.Join(nl, nr) ->
        seq {for l in loop nl do
              for r in loop nr do
                yield JoinToken(l, r)}
      | NodeAC.Constraint(n, (sell, selr)) ->
        loop n
          |> Seq.filter (fun t -> lookup t sell = lookup t selr)
    loop

  let evalProduction factSet ((nodeAC : NodeAC, mapping:Map<Variable, TokenSelector>), action) =
    evalNode factSet nodeAC
      |> Seq.map (fun t -> ReteHelper.evalAction (lookup t) action)
      |> Seq.fold (fun a fact -> Set.add fact a) factSet

  let evalRulesToFix productions factSet =
    let rec loop factSet =
      let factSet' = Seq.fold evalProduction factSet productions
      if factSet' = factSet
      then factSet'
      else loop factSet'
    loop factSet

type ReteInterpreterAC(file:AST.File) =
  let graph = ReteHelper.fileToGraph file
  let acGraph = List.map (fun(ReteHelper.Production(node, action)) -> ReteAlphaConverted.alphaConvert node, action) graph

  let mutable userSet = Set.empty

  interface IInterpreter with
    member __.HasFact fact =
      let systemSet = ReteHelper.evalRulesToFix graph userSet
      Set.contains fact systemSet

    member __.Insert fact =
      userSet <- Set.add fact userSet

    member __.Retract fact =
      userSet <- Set.remove fact userSet

type public ReteInterpreterACProvider() =
  interface IInterpreterProvider with
    member __.GetInterpreter file = ReteInterpreterAC file :> IInterpreter
