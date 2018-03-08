namespace PrologAST

[<AutoOpen>]
module Term =
    open Util
<<<<<<< HEAD
    open System.Text.RegularExpressions
=======
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649

    type Term =
    | NumberTerm of Number
    | StrTerm of Str
    | VarTerm of Var
    | AtomTerm of Atom
<<<<<<< HEAD
    | AppTerm of Symbol * Term list
=======
    | AppTerm of Atom * Term list
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649

    type Interpreter<'a, 'e>  = Term -> Result<'e, 'a>