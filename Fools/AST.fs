namespace Fools

module AST =
  type FactName = string
  type Constant = string
  type Variable = string
  type PatternParameter = PatVar of Variable | PatConst of Constant
  type FactPattern = FactName * PatternParameter list
  type Exp<'TVar> = Var of 'TVar | Const of Constant
  type Action<'TVar> =
    | Insert of FactName * Exp<'TVar> list
  type Rule = FactPattern list * Action<Variable>
  type File = Rule list
