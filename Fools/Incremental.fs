namespace Fools

open AST

module Incremental =
  open ReteAlphaConverted
  type NodeI =
    | Dummy of NodeI[] option ref
    | Alpha of ReteFactPattern * NodeI[] option ref
    | Join of NodeI * NodeI * NodeI[] option ref
    | Constraint of NodeI * ConstraintTest * NodeI[] option ref
    | Memory of NodeI * Set<Token> ref * NodeI[] option ref
    | Prod of NodeI * Action<TokenSelector>

  let mkMem node = Memory(node, ref Set.empty, ref None)

  let rec convertFromAC =
    function
    | NodeAC.Dummy -> Dummy <| ref None
    | NodeAC.Alpha fp -> mkMem <| Alpha(fp, ref None)
    | NodeAC.Join(nl, nr) -> mkMem <| Join(convertFromAC nl, convertFromAC nr, ref None)
    | NodeAC.Constraint(n, test) -> mkMem <| Constraint(convertFromAC n, test, ref None)

  let getChildren =
    let rec loop =
      function
      | Dummy _
      | Alpha _ -> Seq.empty
      | Join(nl, nr, _) as j ->
        Seq.concat [Seq.ofList[nl, j; nr, j]
                    loop nl
                    loop nr]
      | (Constraint(n, _, _) | Prod(n, _) | Memory(n, _, _)) as p ->
        Seq.append (Seq.singleton (n, p)) <| loop n
    loop

  let getChildrenRef =
    function
    | Dummy r -> r
    | Alpha(_, r) -> r
    | Join(_, _, r) -> r
    | Constraint (_, _, r) -> r
    | Memory (_, _, r) -> r
    | _ -> failwith "no children"

  let setChildren childMap =
    Map.iter (fun parent children -> (getChildrenRef parent) := Some <| Set.toArray children) childMap

  let rec choose chooser =
    let rec loop =
      function
      | (Dummy _ | Alpha _) as n -> chooser n
      | Join(nl, nr, _) -> Seq.append (loop nl) (loop nr)
      | Constraint (n, _, _)  | Memory (n, _, _) | Prod (n, _) -> loop n
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
      | Memory(_, tokensRef, _) -> !tokensRef :> _ seq
      | Prod _ -> failwith "prod"
    loop

  let procAgenda activate allFacts token node =
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
      | Memory(_, tokensRef, childrenOptRef) ->
        if activate
        then
          let hasToken = Set.contains token !tokensRef
          tokensRef := Set.add token !tokensRef
          if not hasToken
          then procChildren childrenOptRef token
        else
          failwith "sd"
      | Prod(_, action) -> agenda := (token, action) :: !agenda
    proc None token node
    !agenda

  let mapExpVar f =
    let rec loop =
      function
      | Const c -> Const c
      | Var v -> Var <| f v
    loop

  let mapAction f =
    function
    | Insert(factName, exps) -> Insert(factName, List.map (mapExpVar f) exps)

  type Author =
    | User
    | System of Token * Action<TokenSelector>

  let getFactsInAuthor =
    function
    | User -> Seq.empty
    | System(token, _) -> getFactsInToken token

  type IncrementalInterpreter (file:AST.File) =
    let graph = ReteHelper.fileToGraph file
    let nodes = List.map (fun(ReteHelper.Production(node, action)) -> let nodeAC, mapping = alphaConvert node
                                                                      let mappedAction = mapAction (fun var -> Map.find var mapping) action
                                                                      Prod(convertFromAC nodeAC, mappedAction)) graph
    let childMap = OneToManyMap.ofSeq <| Seq.collect getChildren nodes
    let alphaMappings = Seq.collect getAlphaMap nodes |> build
    let alphaMap = OneToManyMap.ofSeq alphaMappings
    let dummies = Seq.collect getDummies nodes |> Seq.toArray

    let findAlphas ((factName, constants):Fact) =
      let alphaMems = OneToManyMap.findSet factName alphaMap
      alphaMems
        |> Seq.filter (fun(fp, node) -> ReteAlphaConverted.matchConstants constants fp)
        |> Seq.map snd

    let mutable authorMap = Map.empty

    let rec addFactAuthor author fact =
      let authorSet = OneToManyMap.findSet fact authorMap
      authorMap <- OneToManyMap.add fact author authorMap
      if Set.isEmpty authorSet
      then Seq.iter (procFix (FactToken fact)) <| findAlphas fact

    and procFix (token:Token) node =
      let facts = Seq.map fst <| Map.toSeq authorMap
      let agenda = procAgenda true facts token node
      Seq.iter (fun(token, action) -> addFactAuthor (System(token, action)) <| ReteHelper.evalAction (lookup token) action) agenda

    let rec removeFactCascading fact =
      let facts = Seq.map fst <| Map.toSeq authorMap
      Seq.iter (fun node -> ignore <| procAgenda false facts (FactToken fact) node) <| findAlphas fact
      Map.iter (fun authorFact tokens ->
                    tokens
                      |> Set.iter (fun author ->
                                        if Seq.exists ((=)fact) <| getFactsInAuthor author
                                        then removeFactAuthor author authorFact)) authorMap

    and removeFactAuthor author fact =
      authorMap <- OneToManyMap.remove fact author authorMap
      if Set.isEmpty <| OneToManyMap.findSet fact authorMap
      then removeFactCascading fact

    let hasAuthor fact author =
      Set.contains author <| OneToManyMap.findSet fact authorMap

    do
      setChildren childMap
      Seq.iter (procFix UnitToken) dummies

    let mutable insertedUserFacts = Set.empty
    let mutable retractedUserFacts = Set.empty

    member __.FireAllRules () =
      let factsToRetract = retractedUserFacts
      retractedUserFacts <- Set.empty
      let factsToInsert = insertedUserFacts
      insertedUserFacts <- Set.empty
      Seq.iter (removeFactAuthor User) factsToRetract
      Seq.iter (addFactAuthor User) factsToInsert

    member __.HasFact fact =
      not << Set.isEmpty <| OneToManyMap.findSet fact authorMap

    member __.Insert fact =
      retractedUserFacts <- Set.remove fact retractedUserFacts
      if not <| hasAuthor fact User
      then insertedUserFacts <- Set.add fact insertedUserFacts

    member __.Retract fact =
      insertedUserFacts <- Set.remove fact insertedUserFacts
      if hasAuthor fact User
      then retractedUserFacts <- Set.add fact retractedUserFacts

  type public IncrementalProvider() =
    interface IInterpreterProvider with
      member __.GetInterpreter file =
        let interpreter = IncrementalInterpreter file
        interpreter.FireAllRules()
        { new IInterpreter with
            member __.HasFact fact =
              interpreter.FireAllRules()
              interpreter.HasFact fact

            member __.Insert fact = interpreter.Insert fact

            member __.Retract fact = interpreter.Retract fact
        }
