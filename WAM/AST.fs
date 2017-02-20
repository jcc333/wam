namespace WAM

module AST =
  open System
  open WAM.Util
  open FSharp.Collections

  type Term = 
    | Var of String
    | Atom of String
    | App of Term * List<Term>
    
    member this.OccursIn(that: Term): bool = 
      match (this, that) with
      | (Var x, Var y) -> x = y
      | (Atom x, Atom y) -> x = y
      | (x, App(y, terms)) -> x = y || List.exists (fun elt -> x.OccursIn(elt)) terms
      | _ -> false
    
    member this.Contains(that: Term): bool = that.OccursIn(this)
    member this.Replace(a: Term, b: Term): Term = 
      if this = a then b
      else 
        match this with
        | App(fnc, args) -> 
          let fncReplaced = 
            if fnc = a then b
            else fnc
          let argsReplaced = args |> List.map(fun elt -> elt.Replace(a, b))
          App(fncReplaced, argsReplaced)
        | _ -> this

  type Query = Term list
  
  type Clause = 
   | Clause of Term * List<Term>

    member this.Pred: Term = match this with Clause(pred, _) -> pred

    member this.Args: List<Term> = match this with Clause(_, args) -> args
  
  type Pred = Clause list

  type Prog = Map<String, Pred> //map of predicate-names to predicates
