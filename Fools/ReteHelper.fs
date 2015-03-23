namespace Fools

open AST

module ReteHelper =
  type Node =
    | Dummy
    | Alpha of FactPattern
    | Join of Node * Node
  type Production = Production of Node * Action
  type Graph = Production list
  let patternsToNode =
    function
    | [] -> Dummy
    | factPatterns ->  Seq.map Alpha factPatterns |> Seq.reduce (fun l r -> Join(l, r))
  let ruleToProduction (factPatterns, action) = Production(patternsToNode factPatterns, action)
  let fileToGraph (file:AST.File) : Graph = List.map ruleToProduction file

  let evalNode factSet =
    let rec loop =
      function
      | Dummy -> Seq.singleton Map.empty
      | Alpha factPattern -> PatternMatchHelper.findAll factSet factPattern
      | Join(nl, nr) -> seq { for l in loop nl do
                                for r in loop nr do
                                  match PatternMatchHelper.unify l r with
                                  | Some m -> yield m
                                  | None -> () }
    loop

  let evalAction env =
    function
    | Insert(factName, exps) -> (factName, List.map (PatternMatchHelper.evalExp env) exps) : Fact

  let evalProduction factSet (Production(node, action)) =
    evalNode factSet node
      |> Seq.map (fun env -> evalAction env action)
      |> Seq.fold (fun a fact -> Set.add fact a) factSet

  let evalRulesToFix productions factSet =
    let rec loop factSet =
      let factSet' = Seq.fold evalProduction factSet productions
      if factSet' = factSet
      then factSet'
      else loop factSet'
    loop factSet
