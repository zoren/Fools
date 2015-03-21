namespace Fools

[<AutoOpen>]
module FactHelper =
  type Fact = AST.FactName
  let mkFact (factName:AST.FactName) args =
    if args <> []
    then failwith "args not yet supported"
    else factName : Fact

open AST

type Author = User | System

type Interpreter(file:AST.File) =
  let mutable factAuthors = Map.empty : Map<Fact, Set<Author>>

  let hasAuthor author fact = Set.contains author <| OneToManyMap.findSet fact factAuthors

  let cartesian ys xs = xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> Seq.append y (Seq.singleton x) ))
  let cartesianN = Seq.fold cartesian (Seq.singleton Seq.empty)

  let matches (factPattern:FactPattern) (fact:Fact) = fact = factPattern

  let findFacts (factPattern:FactPattern) =
    Map.toSeq factAuthors |> Seq.map fst |> Seq.filter (matches factPattern)

  let rec evalRules() =
    let evalRule ((assumptions, action):Rule) =
      let combinations = cartesianN <| Seq.map findFacts assumptions
      Seq.iter (fun facts -> if evalAction action then evalRules()) combinations
    Seq.iter evalRule file

  and evalAction =
    function
    | Insert fact ->
      let authors = OneToManyMap.findSet fact factAuthors
      if not <| Set.contains System authors
      then factAuthors <- OneToManyMap.add fact System factAuthors
      Set.isEmpty authors

  member __.HasFact (fact:Fact) : bool =
    evalRules()
    not << Set.isEmpty <| OneToManyMap.findSet fact factAuthors

  member __.Insert (fact:Fact) : unit =
    if not <| hasAuthor User fact
    then factAuthors <- OneToManyMap.add fact User factAuthors

  member __.Retract (fact:Fact) : unit =
    if hasAuthor User fact
    then factAuthors <- OneToManyMap.remove fact User factAuthors
