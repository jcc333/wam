namespace PrologAST

[<AutoOpen>]
module Number =
    open System

    type Number =
    | Int of int64
    | Double of double
