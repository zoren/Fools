namespace Fools

module AST =
  type FactName = string  
  type Constant = string
  type Variable = string
  type PatternParameter = PatVar of Variable | PatConst of Constant
  type FactPattern = FactName * PatternParameter list
  type Exp = Var of Variable | Const of Constant
  type Action =
    | Insert of FactName * Exp list
  type Rule = FactPattern list * Action
  type File = Rule list
