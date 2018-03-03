// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open PrologAST.Term
open PrologFrontend.Parser

open Util

let readPrompt(s: string) = Console.Write s ; Console.ReadLine() 

let parseAndPrint =
    (run parser) >>
    (fun t -> t.ToString) >>
    Console.Write >>
    Console.WriteLine

let rec until pred prompter evaluator = 
    let result = prompter() 
    if not(pred result) then 
        evaluator result 
        until pred prompter evaluator 

let runRepl() =  
    let quittingTime(s: String) = "quit" = s.ToLower()
    let prompt() = readPrompt "PrologFrontend>>> "
    until quittingTime prompt parseAndPrint

[<EntryPoint>] 
let main(args: string[]) = 
    runRepl() 
    0
