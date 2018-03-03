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

    module Atom =
        let emptyTuple<'a> : Parser<Atom, 'a> = pstring "{}" >>% EmptyList
        let emptyList<'a> : Parser<Atom, 'a> = pstring "[]" >>% EmptyTuple

        let alphaNumericOrUnderscore<'a> : Parser<Atom, 'a> = regex "[a-z][a-zA-Z0-9_]*" |>> Symbol

        let specialCharacters<'a> : Parser<Atom, 'a> = anyOf "#$&*-./:<->?@^~\\" |> many1Chars |>> Symbol

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
            pstring "\\\n" |>> discard

        let atomBody<'a> : Parser<char option list, 'a> = (many (normalChar <|> lineBreak <|> escapedChar))

        let quoted<'a> : Parser<Atom, 'a> = between (pstring "'") (pstring "'") atomBody |>> string |>> Symbol

        let parser<'a> : Parser<Atom, 'a> = emptyTuple <|> emptyList <|> alphaNumericOrUnderscore <|> specialCharacters <|> quoted


    module Number =
        let float<'a> : Parser<Number, 'a> = regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" |>> Double.Parse |>> Double
        let integer<'a> : Parser<Number, 'a> = regex "[-+]?[0-9]+" |>> Int64.Parse |>> Int
        let parser<'a> : Parser<Number, 'a> = float <|> integer

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
        let parser<'a> : Parser<Var, 'a> = anon <|> named

    module Term =
        open Atom
        open Number
        open Str
        open Var

        let parser<'a> : Parser<Term, 'a> =
            let rec loop =
                (Number.parser |>> Term.NumberTerm) <|>
                (Str.parser |>> StrTerm) <|>
                (Var.parser |>> VarTerm) <|>
                (Atom.parser |>> AtomTerm) <|>
                (app |>> AppTerm)
            and app = FParsec.Primitives.parse.Delay(fun() ->
                let lparen = pstring "("
                let rparen = pstring ")"
                let parens = between lparen rparen
                let arguments: Parser<Term list, 'a> = sepBy loop (pstring ",")
                (Atom.parser .>>. parens arguments)
                )
            loop

    let parser<'a> : Parser<Term, 'a> = Term.parser
