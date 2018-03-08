namespace PrologAST

[<AutoOpen>]
module Atom =

    type Symbol = Symbol of string

    type Atom =
    | SymbolAtom of Symbol
    | EmptyList
    | EmptyTuple
