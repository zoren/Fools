namespace Fools

open AST

type Author = User | System

module PatternMatchHelper =
  let tryGetMatch (c:Constant) =
    function
    | PatConst pc -> if c = pc then Some Map.empty else None
    | PatVar v -> Some <| Map.ofList [v, c]

  let unify (l:Map<Variable,Constant>) (r:Map<Variable,Constant>) =
    let rec loop =
      function
      | [] -> Some r
      | (v, c) :: ll ->
        match Map.tryFind v r with
        | Some vr when v <> vr -> None
        | _ -> Option.map (fun m -> Map.add v c m) (loop ll)
    loop <| Map.toList l

  let unifyN = Seq.fold (fun a e -> Option.bind (fun l -> unify e l) a) (Some Map.empty)

  let rec matchPatterns factPattern args =
    match factPattern, args with
    | [], [] -> Some Map.empty
    | pp::pps, c::cs ->
      match tryGetMatch c pp, matchPatterns pps cs with
      | Some l, Some r -> unify l r
      | _ -> None
    | _ -> failwith "arity mismatch"

  let matches ((patName, patternParams):FactPattern) ((factName, args):Fact) =
    if patName = factName
    then matchPatterns patternParams args
    else None

  let cartesian ys xs = xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> Seq.append y (Seq.singleton x) ))
  let cartesianN = Seq.fold cartesian (Seq.singleton Seq.empty)

  let findAllMatches (facts : Fact seq) (factPatterns:FactPattern list) : Map<Variable, Constant> seq =
    let findAll pat = Seq.choose (matches pat) facts
    Seq.choose unifyN << cartesianN <| Seq.map findAll factPatterns

type NaiveInterpreter(file:AST.File) =
  let mutable factAuthors = Map.empty : Map<Fact, Set<Author>>

  let hasAuthor author fact = Set.contains author <| OneToManyMap.findSet fact factAuthors

  let cartesian ys xs = xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> Seq.append y (Seq.singleton x) ))
  let cartesianN = Seq.fold cartesian (Seq.singleton Seq.empty)

  let evalExp env =
    function
    | Const c -> c
    | Var v -> Map.find v env

  let evalAction env =
    function
    | Insert(factName, exps) ->
      let fact = factName, List.map (evalExp env) exps
      if hasAuthor System fact
      then false
      else
        let noFact = Set.isEmpty <| OneToManyMap.findSet fact factAuthors
        factAuthors <- OneToManyMap.add fact System factAuthors
        noFact

  let rec evalRules() =
    let evalRule ((assumptions, action):Rule) =
      Seq.iter (fun env -> if evalAction env action then evalRules())
        <| PatternMatchHelper.findAllMatches (Map.toSeq factAuthors |> Seq.map fst) assumptions
    Seq.iter evalRule file

  interface IInterpreter with
    member __.HasFact (fact:Fact) : bool =
      evalRules()
      not << Set.isEmpty <| OneToManyMap.findSet fact factAuthors

    member __.Insert (fact:Fact) : unit =
      if not <| hasAuthor User fact
      then factAuthors <- OneToManyMap.add fact User factAuthors

    member __.Retract (fact:Fact) : unit =
      if hasAuthor User fact
      then factAuthors <- OneToManyMap.remove fact User factAuthors

type public InterpreterProvider() =
  interface IInterpreterProvider with
    member __.GetInterpreter file = NaiveInterpreter file :> IInterpreter