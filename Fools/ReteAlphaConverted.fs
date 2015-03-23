namespace Fools

open AST

module ReteAlphaConverted =
  type RetePatternParam = ReteAnything | RetePatConst of Constant
  type ReteFactPattern = FactName * RetePatternParam list
  type TokenSelector =
    | Left of TokenSelector
    | Right of TokenSelector
    | FactIndex of int
  type NodeAC =
    | Dummy
    | Alpha of ReteFactPattern * Set<int * int>
    | Join of NodeAC * NodeAC * Set<TokenSelector * TokenSelector>

  let patParamToRete =
    function
    | PatVar _ -> ReteAnything
    | PatConst c -> RetePatConst c

  let factPatternToAlpha (patternParams:PatternParameter list) =
    let toMap i =
      function
      | PatVar v -> Some(v, i)
      | PatConst _ -> None
    Seq.mapi toMap patternParams
      |> Seq.choose id
      |> Seq.fold (fun (eqlist, accMap) (v, i) -> let eqlist' =
                                                    match Map.tryFind v accMap with
                                                    | Some i' -> Set.add (i, i')  eqlist
                                                    | None -> eqlist
                                                  eqlist', Map.add v i accMap) (Set.empty, Map.empty)

  let rec alphaConvert =
    function
    | ReteHelper.Dummy -> NodeAC.Dummy, Map.empty
    | ReteHelper.Alpha(factName, patternParams) ->
      let eqlist, map = factPatternToAlpha patternParams
      NodeAC.Alpha((factName, List.map patParamToRete patternParams), eqlist), Map.map (fun _ i -> FactIndex i) map
    | ReteHelper.Join(l, r) ->
      let nl, ml = alphaConvert l
      let nr, mr = alphaConvert r
      let m' =
        Seq.append
          (Map.toSeq ml |> Seq.map (fun (v, i) -> v, Left i))
          (Map.toSeq mr |> Seq.map (fun (v, i) -> v, Right i)) |> Map.ofSeq
      let eqList = Map.toArray ml |> Seq.choose (fun(vl, il) -> Option.map (fun ir -> il, ir) <| Map.tryFind vl mr) |> Set.ofSeq
      NodeAC.Join(nl, nr, Set.empty), m'

  type Token =
    | UnitToken
    | FactToken of Fact
    | JoinToken of Token * Token

  let matchPatternParam c =
    function
    | ReteAnything -> true
    | RetePatConst c' -> c = c'

  let matchPattern (factName, reteFactPattern) ((factName', consts)) =
    if List.length reteFactPattern <> List.length consts
    then failwith "arity mismatch"
    factName = factName' && List.forall2 matchPatternParam consts reteFactPattern

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
      | NodeAC.Alpha(reteFactPattern, indexEqs) ->
        factSet
          |> Seq.filter (matchPattern reteFactPattern)
          |> Seq.filter (fun (_, fact) -> Seq.forall (fun(i, i') -> List.nth fact i = List.nth fact i') indexEqs)
          |> Seq.map FactToken
      | NodeAC.Join(nl, nr, tokenEqs) ->
        seq {for l in loop nl do
              for r in loop nr do
                yield JoinToken(l, r)}
          |> Seq.filter (fun t -> Seq.forall (fun (tokenEql, tokenEqr) -> lookup t tokenEql = lookup t tokenEqr) tokenEqs)
    loop

  let evalProduction factSet ((nodeAC : NodeAC, mapping:Map<Variable, TokenSelector>), action) =
    evalNode factSet nodeAC
      |> Seq.map (fun t -> Map.map (fun var selector -> lookup t selector) mapping)
      |> Seq.map (fun env -> ReteHelper.evalAction env action)
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
