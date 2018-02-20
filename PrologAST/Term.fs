namespace PrologAST

[<AutoOpen>]
module Term =
    type Term =
    | NumberTerm of Number
    | StrTerm of Str
    | VarTerm of Var
    | AtomTerm of Atom
    | AppTerm of Atom * Term list