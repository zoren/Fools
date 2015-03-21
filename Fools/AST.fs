namespace Fools

module AST =
  type FactName = string  
  type FactType = FactName
  type FactPattern = FactName
  type Action =
    | Insert of FactName
  type Rule = FactPattern list * Action
  type File = FactType list * Rule list
