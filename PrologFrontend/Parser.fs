namespace PrologFrontend

[<AutoOpen>]
module Parser =
    open FParsec
    open FParsec.CharParsers
    open FParsec.Error
    open FParsec.Internals
    open FParsec.Primitives
    open FParsec.Internal

    open System

    open PrologAST.Atom
    open PrologAST.Number
    open PrologAST.Str
    open PrologAST.Var
    open PrologAST.Term

<<<<<<< HEAD
    module Symbol = 
        let alphaNumericOrUnderscore<'a> : Parser<Symbol, 'a> = regex "[a-z][a-zA-Z0-9_]*" |>> Symbol
        let specialCharacters<'a> : Parser<Symbol, 'a> = anyOf "#$&*-./:<->?@^~\\" |> many1Chars |>> Symbol
        let parser<'a> : Parser<Symbol, 'a> = spaces >>. (alphaNumericOrUnderscore <|> specialCharacters)

    module Atom =
        let emptyTuple<'a> : Parser<Atom, 'a> = pstring "{}" >>% EmptyList
        let emptyList<'a> : Parser<Atom, 'a> = pstring "[]" >>% EmptyTuple
=======
    module Atom =
        let emptyTuple<'a> : Parser<Atom, 'a> = pstring "{}" >>% EmptyList
        let emptyList<'a> : Parser<Atom, 'a> = pstring "[]" >>% EmptyTuple

        let alphaNumericOrUnderscore<'a> : Parser<Atom, 'a> = regex "[a-z][a-zA-Z0-9_]*" |>> Symbol

        let specialCharacters<'a> : Parser<Atom, 'a> = anyOf "#$&*-./:<->?@^~\\" |> many1Chars |>> Symbol
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649

        let normalChar<'a> : Parser<char option, 'a> = satisfy (fun c -> c <> '\\' && c <> ''') |>> Some
        let unescape(c: char): char = match c with
                         | 'a' -> '\a'
                         | 'b' -> '\b'
                         | 'f' -> '\f'
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | 'v' -> '\v'
                         | '\\'-> '\\'
                         | ''' -> '''
                         | c   -> c
        let escapedChar<'a> : Parser<char option, 'a> = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape |>> Some)

        let lineBreak<'a> : Parser<char option, 'a> =
            let discard _ = None
            regex "\\\n" |>> discard

        let atomBody<'a> : Parser<char option list, 'a> = (many (normalChar <|> lineBreak <|> escapedChar))

<<<<<<< HEAD
        let quoted<'a> : Parser<Atom, 'a> = between (pstring "'") (pstring "'") atomBody |>> string |>> Symbol |>> SymbolAtom

        let symbolAtom<'a> : Parser<Atom, 'a> = Symbol.parser |>> SymbolAtom

        let parser<'a> : Parser<Atom, 'a> = spaces >>. (emptyTuple <|> emptyList <|> symbolAtom <|> quoted)


    module Number =
        let float<'a> : Parser<Number, 'a> = regex "[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?" |>> Double.Parse |>> Double
        let integer<'a> : Parser<Number, 'a> = regex "[-+]?[0-9]+" |>> Int64.Parse |>> Int
        let parser<'a> : Parser<Number, 'a> = spaces >>. (float <|> integer)
=======
        let atomBody<'a> : Parser<char option list, 'a> = (many (normalChar <|> lineBreak <|> escapedChar))

        let quoted<'a> : Parser<Atom, 'a> = between (pstring "'") (pstring "'") atomBody |>> string |>> Symbol

        let parser<'a> : Parser<Atom, 'a> = emptyTuple <|> emptyList <|> alphaNumericOrUnderscore <|> specialCharacters <|> quoted


    module Number =
        let float<'a> : Parser<Number, 'a> = regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" |>> Double.Parse |>> Double
        let integer<'a> : Parser<Number, 'a> = regex "[-+]?[0-9]+" |>> Int64.Parse |>> Int
        let parser<'a> : Parser<Number, 'a> = float <|> integer
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649

    module Str =
        let parser<'a> : Parser<Str, 'a> =
            let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
            let unescape c = match c with
                             | 'n' -> '\n'
                             | 'r' -> '\r'
                             | 't' -> '\t'
                             | c   -> c
            let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
            between (pstring "\"") (pstring "\"")
                    (manyChars (normalChar <|> escapedChar)) |>> Str
            
    module Var =
        let anon<'a> : Parser<Var, 'a> = pstring "_" >>% Anon
        let named<'a> : Parser<Var, 'a> = regex "[A-Z_][a-zA-Z0-9_]*" |>> Named
<<<<<<< HEAD
        let parser<'a> : Parser<Var, 'a> = spaces >>. (anon <|> named)
=======
        let parser<'a> : Parser<Var, 'a> = anon <|> named
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649

    module Term =
        open Atom
        open Number
        open Str
        open Var

        let parser<'a> : Parser<Term, 'a> =
<<<<<<< HEAD
            let nonAppAtom = Atom.parser .>>? notFollowedBy (pstring "(") |>> AtomTerm
            let rec loop =
                (Number.parser |>> Term.NumberTerm) <|>
                nonAppAtom <|>
                (Str.parser |>> StrTerm) <|>
                (Var.parser |>> VarTerm) <|>
                app 
=======
            let rec loop =
                (Number.parser |>> Term.NumberTerm) <|>
                (Str.parser |>> StrTerm) <|>
                (Var.parser |>> VarTerm) <|>
                (Atom.parser |>> AtomTerm) <|>
                (app |>> AppTerm)
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649
            and app = FParsec.Primitives.parse.Delay(fun() ->
                let lparen = pstring "("
                let rparen = pstring ")"
                let parens = between lparen rparen
<<<<<<< HEAD
                let commaSep = spaces >>. (pstring ",")
                let arguments: Parser<Term list, 'a> = sepBy loop commaSep
                (Symbol.parser .>>. parens arguments) |>> AppTerm
=======
                let arguments: Parser<Term list, 'a> = sepBy loop (pstring ",")
                (Atom.parser .>>. parens arguments)
>>>>>>> 2bac5e752b22ab3da9e1d218219dc7224c32b649
                )
            loop

    let parser<'a> : Parser<Term, 'a> = Term.parser
