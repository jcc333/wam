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
        let emptyTuple = pstring "{}" >>% Atom.EmptyList
        let emptyList = pstring "[]" >>% Atom.EmptyTuple

        let alphaNumericOrUnderscore = regex "[a-z][a-zA-Z0-9_]*" |>> Atom.Symbol

        let specialCharacters = anyOf "#$&*-./:<->?@^~\\" |> many1Chars |>> Atom.Symbol

        let normalChar = satisfy (fun c -> c <> '\\' && c <> ''') |>> Some
        let unescape c = match c with
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
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape |>> Some)

        let lineBreak =
            let discard _ = None
            pstring "\\\n" |>> discard

        let atomBody = (many (normalChar <|> lineBreak <|> escapedChar))

        let quoted = between (pstring "'") (pstring "'") atomBody |>> string |>> Atom.Symbol

        let parser = emptyTuple <|> emptyList <|> alphaNumericOrUnderscore <|> specialCharacters <|> quoted


    module Number =
        let float = regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" |>> Double.Parse |>> Number.Double
        let integer = regex "[-+]?[0-9]+" |>> Int64.Parse |>> Number.Int
        let parser = float <|> integer

    module Str =
        let parser =
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
        let anon = pstring "_" >>% Anon
        let named = regex "[A-Z_][a-zA-Z0-9_]*" |>> Named
        let parser = anon <|> named

    module Term =
        open Atom
        open Number
        open Str
        open Var

        let rec parser = 
            (Number.parser |>> Term.NumberTerm) <|>
            (Str.parser |>> StrTerm) <|>
            (Var.parser |>> VarTerm) <|>
            (Atom.parser |>> AtomTerm) <|>
            (app |>> AppTerm)
        and app = FParsec.Primitives.parse.Delay(fun() ->
            let lparen = pstring "("
            let rparen = pstring ")"
            let parens = between lparen rparen
            let arguments: Parser<Term list, obj> = sepBy parser (pstring ",")
            (Atom.parser .>>. parens arguments)
            )

    let parser<'t> = Term.parser
