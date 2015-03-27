namespace Fools

open AST

module Incremental =
  open ReteAlphaConverted
  type NodeI =
    | Dummy of NodeI[] option ref
    | Alpha of ReteFactPattern * NodeI[] option ref
    | Join of NodeI * NodeI * NodeI[] option ref
    | Constraint of NodeI * ConstraintTest * NodeI[] option ref
    | Prod of NodeI * Map<Variable, TokenSelector> * Action

  let rec convertFromAC =
    function
    | NodeAC.Dummy -> Dummy <| ref None
    | NodeAC.Alpha fp -> Alpha(fp, ref None)
    | NodeAC.Join(nl, nr) -> Join(convertFromAC nl, convertFromAC nr, ref None)
    | NodeAC.Constraint(n, test) -> Constraint(convertFromAC n, test, ref None)

  let getChildren =
    let rec loop =
      function
      | Dummy _
      | Alpha _ -> Seq.empty
      | Join(nl, nr, _) as j ->
        Seq.concat [Seq.ofList[nl, j; nr, j]
                    loop nl
                    loop nr]
      | (Constraint(n, _, _) | Prod(n, _, _)) as p ->
        Seq.append (Seq.singleton (n, p)) <| loop n
    loop

  let getChildrenRef =
    function
    | Dummy r -> r
    | Alpha(_, r) -> r
    | Join(_, _, r) -> r
    | Constraint (_, _, r) -> r
    | _ -> failwith "no children"

  let updateChildren updater =
    let rec loop node =
      match node with
      | Dummy r -> r := updater node
      | Alpha(_, r) -> r := updater node
      | Join(nl, nr, r) ->
        loop nl
        loop nr
        r := updater node
      | Constraint (n, _, _)  | Prod (n, _, _) -> loop n
    loop

  let clearChildren = updateChildren (fun _ -> None)

  let setChildren childMap =
    Map.iter (fun parent children -> (getChildrenRef parent) := Some <| Set.toArray children) childMap

  let rec choose chooser =
    let rec loop =
      function
      | (Dummy _ | Alpha _) as n -> chooser n
      | Join(nl, nr, _) -> Seq.append (loop nl) (loop nr)
      | Constraint (n, _, _)  | Prod (n, _, _) -> loop n
    loop

  let rec getAlphaMap =
    let f =
      function
      | Alpha(fp, _) as n -> Seq.singleton (fp, n)
      | _ -> Seq.empty
    choose f

  let rec getDummies =
    let chooser =
      function
      | Dummy _ as d -> Seq.singleton d
      | _ -> Seq.empty
    choose chooser

  let build (s : (ReteFactPattern * _) seq) = s |> Seq.map (fun((factName, pps), v) -> factName, (pps, v))

  let evalTest ((tl, tr):ConstraintTest) token = lookup token tl = lookup token tr

  let evalNode factSet =
    let rec loop =
      function
      | Dummy _ -> Seq.singleton UnitToken
      | Alpha(reteFactPattern, _) ->
        factSet
          |> Seq.filter (matchPattern reteFactPattern)
          |> Seq.map FactToken
      | Join(nl, nr, _) ->
        seq {for l in loop nl do
              for r in loop nr do
                yield JoinToken(l, r)}
      | Constraint(n, test, _) ->
        loop n
          |> Seq.filter (evalTest test)
      | Prod _ -> failwith "prod"
    loop

  let procAgenda allFacts token node =
    let agenda = ref []
    let rec proc parentOpt token node =
      let procChildren childrenOptRef token = Seq.iter (proc (Some node) token) << Option.get <| !childrenOptRef
      match node with
      | Dummy childrenOptRef ->
        procChildren childrenOptRef token
      | Alpha(fp, childrenOptRef) ->
        procChildren childrenOptRef token
      | Join(nl, nr, childrenOptRef) ->
        let parent = Option.get parentOpt
        if System.Object.ReferenceEquals( parent, nl )
        then
          // when activated from left
          evalNode allFacts nr
            |> Seq.map (fun tr -> JoinToken(token, tr))
            |> Seq.iter (procChildren childrenOptRef)
        else
          if not <| System.Object.ReferenceEquals( parent, nr )
          then failwith "parent neither left or right"
          evalNode allFacts nl
            |> Seq.map (fun tl -> JoinToken(tl, token))
            |> Seq.iter (procChildren childrenOptRef)
      | Constraint(_, test, childrenOptRef) ->
        if evalTest test token
        then procChildren childrenOptRef token
      | Prod(_, mapping,  action) -> agenda := (token, mapping, action) :: !agenda
    proc None token node
    !agenda

  type IncrementalInterpreter (file:AST.File) = //nodes : NodeI seq) =
    let graph = ReteHelper.fileToGraph file
    let nodes = List.map (fun(ReteHelper.Production(node, action)) -> let nodeAC, mapping = alphaConvert node
                                                                      Prod(convertFromAC nodeAC, mapping, action)) graph
    let childMap = OneToManyMap.ofSeq <| Seq.collect getChildren nodes
    let alphaMappings = Seq.collect getAlphaMap nodes |> build
    let alphaMap = OneToManyMap.ofSeq alphaMappings
//    let dummies = Seq.collect getDummies nodes |> Seq.toArray

    let findAlphas ((factName, constants):Fact) =
      let alphaMems = OneToManyMap.findSet factName alphaMap
      alphaMems
        |> Seq.filter (fun(fp, node) -> ReteAlphaConverted.matchConstants constants fp)
        |> Seq.map snd

    let rec processConflict factSet (token, mapping, action) =
      let fact = ReteHelper.evalAction (tokenToEnv token mapping) action
      procFix factSet fact

    and procFix factSet fact =
      if Set.contains fact factSet
      then factSet
      else
        let alphaNodes = findAlphas fact
        let procNode factSet node =
          let agenda = procAgenda factSet (FactToken fact) node
          Seq.fold processConflict factSet agenda
        Seq.fold procNode (Set.add fact factSet) alphaNodes

    let mutable userFacts = Set.empty
    let mutable systemFacts = Set.empty
    interface IInterpreter with
      member __.HasFact fact = Set.contains fact userFacts || Set.contains fact systemFacts

      member __.Insert fact =
        let facts = Set.union userFacts systemFacts
        setChildren childMap
        systemFacts <- procFix facts fact
        Seq.iter clearChildren nodes
        userFacts <- Set.add fact userFacts

      member __.Retract fact =
        //userFacts <- Set.remove fact userFacts
        failwith "not implemented"

  type public IncrementalProvider() =
    interface IInterpreterProvider with
      member __.GetInterpreter file = IncrementalInterpreter file :> IInterpreter
