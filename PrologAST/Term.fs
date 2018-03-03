namespace PrologAST

[<AutoOpen>]
module Term =
    open Util

    type Term =
    | NumberTerm of Number
    | StrTerm of Str
    | VarTerm of Var
    | AtomTerm of Atom
    | AppTerm of Atom * Term list

    type Interpreter<'a, 'e>  = Term -> Result<'e, 'a>