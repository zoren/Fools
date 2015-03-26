namespace Fools

open AST

module Incremental =
  open ReteAlphaConverted
  type NodeI =
    | Dummy of NodeI[] option ref
    | Alpha of ReteFactPattern * Set<int * int> * NodeI[] option ref
    | Join of NodeI * NodeI * Set<TokenSelector * TokenSelector> * NodeI[] option ref
    | Prod of NodeI * Map<Variable, TokenSelector> * Action
  
  let rec convertFromAC =
    function
    | NodeAC.Dummy -> Dummy <| ref None
    | NodeAC.Alpha(fp, s) -> Alpha(fp, s, ref None)
    | NodeAC.Join(nl, nr, s) -> Join(convertFromAC nl, convertFromAC nr, s, ref None)

  let getChildren =
    let rec loop =
      function
      | Dummy _
      | Alpha _ -> Seq.empty
      | Join(nl, nr, _, _) as j ->
        Seq.concat [Seq.ofList[nl, j; nr, j]
                    loop nl
                    loop nr]
      | Prod(n, _, _) as p -> Seq.append (Seq.singleton (n, p)) <| loop n
    loop

  let updateChildren updater =
    let rec loop node =
      match node with
      | Dummy r -> r := updater node
      | Alpha(_, _, r) -> r := updater node
      | Join(nl, nr, _, r) ->
        loop nl
        loop nr
        r := updater node
      | Prod (n, _, _) -> loop n
    loop

  let clearChildren = updateChildren (fun _ -> None)

  let setChildren childMap = updateChildren (fun node -> Some << Set.toArray<| OneToManyMap.findSet node childMap)

  let rec getAlphaMap =
    function
    | Dummy _ -> Seq.empty
    | Alpha(fp, _, _) as n -> Seq.singleton (fp, n)
    | Join(nl, nr, _, _) -> Seq.append (getAlphaMap nl) (getAlphaMap nr)
    | Prod(n, _, _) -> getAlphaMap n

  let rec getDummies =
    function
    | Dummy _ as n -> Seq.singleton n
    | Alpha(fp, _, _) as n -> Seq.empty
    | Join(nl, nr, _, _) -> Seq.append (getDummies nl) (getDummies nr)
    | Prod(n, _, _) -> getDummies n
  
  let build (s : (ReteFactPattern * _) seq) = s |> Seq.map (fun((factName, pps), v) -> factName, (pps, v))
   
  let evalNode factSet =
    let rec loop =
      function
      | Dummy _ -> Seq.singleton UnitToken
      | Alpha(reteFactPattern, indexEqs, _) ->
        factSet
          |> Seq.filter (matchPattern reteFactPattern)
          |> Seq.filter (fun (_, fact) -> Seq.forall (fun(i, i') -> List.nth fact i = List.nth fact i') indexEqs)
          |> Seq.map FactToken
      | Join(nl, nr, tokenEqs, _) ->
        seq {for l in loop nl do
              for r in loop nr do
                yield JoinToken(l, r)}
          |> Seq.filter (fun t -> Seq.forall (fun (tokenEql, tokenEqr) -> lookup t tokenEql = lookup t tokenEqr) tokenEqs)
      | Prod _ -> failwith "prod"
    loop
    
  let procAgenda allFacts token node =
    let agenda = ref []
    let rec proc parentOpt token node =
      let procChildren childrenOptRef token = Seq.iter (proc (Some node) token) << Option.get <| !childrenOptRef
      match node with
      | Dummy childrenOptRef ->
        procChildren childrenOptRef token
      | Alpha(fp, s, childrenOptRef) ->
        procChildren childrenOptRef token
      | Join(nl, nr, s, childrenOptRef) ->
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
        Seq.iter (setChildren childMap) nodes
        systemFacts <- procFix facts fact
        Seq.iter clearChildren nodes
        userFacts <- Set.add fact userFacts

      member __.Retract fact =
        //userFacts <- Set.remove fact userFacts
        failwith "not implemented"
    
  type public IncrementalProvider() =
    interface IInterpreterProvider with
      member __.GetInterpreter file = IncrementalInterpreter file :> IInterpreter
