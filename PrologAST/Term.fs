namespace PrologAST

[<AutoOpen>]
module Term =
    open Util
    open System.Text.RegularExpressions

    type Term =
    | NumberTerm of Number
    | StrTerm of Str
    | VarTerm of Var
    | AtomTerm of Atom
    | AppTerm of Symbol * Term list

    type Interpreter<'a, 'e>  = Term -> Result<'e, 'a>