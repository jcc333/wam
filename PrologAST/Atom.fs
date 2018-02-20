namespace PrologAST

[<AutoOpen>]
module Atom =
    type Atom =
    | Symbol of string
    | EmptyList
    | EmptyTuple
