namespace PrologAST

[<AutoOpen>]
module Var =
    type Var = 
    | Named of string
    | Anon
