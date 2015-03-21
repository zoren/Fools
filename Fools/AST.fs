namespace Fools

module AST =
  type FactName = string  
  type FactPattern = FactName
  type Action =
    | Insert of FactName
  type Rule = FactPattern list * Action
  type File = Rule list
