// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open PrologAST.Term
open PrologFrontend.Parser

open Util

let readPrompt(s: String) = Console.Write s ; Console.ReadLine() 

let parse = run parser

let print = function
| ParserResult.Success(term, _, _) -> printfn "%A\n" term
| ParserResult.Failure(errorAsString, _, _) -> printfn "%s" errorAsString

let rec until pred prompter evaluator = 
    let result = prompter() 
    if not(pred result) then 
        evaluator result 
        until pred prompter evaluator 

let runRepl() =  
    let quittingTime(s: String) = "quit" = s.ToLower()
    let prompt() = readPrompt "PrologFrontend>>> "
    let parseShowPrint = parse >> print
    until quittingTime prompt parseShowPrint

[<EntryPoint>] 
let main(args: String[]) = 
    runRepl() 
    0
