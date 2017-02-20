namespace WAM

[<AutoOpen>]
module Interpret = 
  open System
  open WAM.Util
  //open System.Collections.Generic
  open WAM.AST
  open FSharp.Collections

  [<AutoOpen>]
  module Substitution =
    type Substitution(data: Map<Term, List<Term>>) = 
      static member empty = Substitution(Map.empty)

      member this.Update(k, v) =
        this.TryFind(k)
        |> Option.map(fun states -> data.Add(k, v::states))
        |> Option.getOrElse(data.Add(k, [v]))
        |> Substitution

      member this.Unwind(k: Term): Substitution =
        this.TryFind(k)
        |> Option.map (function _::states -> data.Add(k, states) | _ -> data.Remove(k))
        |> Option.getOrElse(data)
        |> Substitution
    
      member this.Remove(k: Term): Substitution = Substitution(data.Remove(k))

      member this.TryFind(k: Term): Option<List<Term>> = data.TryFind(k)

      member this.Find(k: Term): List<Term> = data.TryFind(k).GetOrElse([])

      member this.Current(k:Term): Option<Term> = this.TryFind(k) |> Option.bind List.headOption

    let update (k: Term) (v: Term) (s: Substitution): Substitution = s.Update(k, v)
    let unwind (k: Term) (s: Substitution): Substitution           = s.Unwind(k)
    let remove (k: Term) (v: Term) (s: Substitution): Substitution = s.Remove(k)
    let tryFind (k: Term) (s: Substitution): Option<List<Term>>    = s.TryFind(k)
    let find (k: Term) (s: Substitution): List<Term>               = s.Find(k)
    let current (k: Term) (s: Substitution): Option<Term>          = s.Current(k)

  type DB = Map<String, Pred> //registry of named predicates//Dictionary<String, Pred> //registry of named predicates
  
  type Answer = Result<String, Option<Substitution>>
  
  type Prolog() = 
    let mutable db  = Map.empty
    let mutable sub = Substitution.empty

    let mostGeneralUnifier(t1: Term, t2: Term, sub: Substitution): Substitution option = 
      let rec loop (result: Substitution) (stack: List<Term * Term>): Substitution option = 
        if stack.IsEmpty then Some result
        else 
          match stack.Head with
          | (x, y) when x = y -> loop result stack.Tail
          | (Var _ as x, y) when not(x.OccursIn(y)) -> 
            stack.Tail
            |> List.map(fun (l, r) -> (l.Replace(x, y), r.Replace(x, y)))
            |> loop (result.Update(x, y))
          | (x, (Var _ as y)) when not(y.OccursIn(x)) -> 
            stack.Tail
            |> List.map(fun (l, r) -> (l.Replace(y, x), r.Replace(y, x)))
            |> loop (result.Update(y, x))
          | (App(x, xs), App(y, ys)) when xs.Length = ys.Length -> 
            loop result ((x, y) :: (List.append (List.zip xs ys) stack.Tail))
          | _ -> None
      loop sub [ (t1, t2) ]

    member this.DB 
      with public get k = db.TryFind(k)
      and public set k v = db <- db.Add(k, v)
    
    member this.Declare op pred = this.DB(op) <- pred
    
    member this.Sub 
      with public get k = sub.TryFind(k)
      and public set k v = sub <- sub.Update(k, v)
    
    member this.PredicateResult(op: String): Result<String, Pred> = 
      this.DB(op).ToResult(sprintf "Predicate %s not in the database" op)
    
    member private this.Eval(sub: Substitution, query: Query, depth: int): Answer = 
      let freshClause(n: int, cls: Clause): Clause = 
        let rec aux = 
          function 
          | App(fn, terms) -> App(aux fn, List.map aux terms)
          | Var v -> Var(sprintf "%s__%d" v n)
          | atom -> atom
        Clause(aux cls.Pred, List.map aux cls.Args)
      if query.IsEmpty then Answer.Success(None)
      else 
        match query.Head with
        | Var v -> Error(sprintf "Variable (%s) as a goal." v)
        | Atom a -> Answer.Success(Some(Substitution.empty))
        | App(Atom op, _) as app -> 
          let rec tryEval (s: Substitution) (gs: Query) (n: int) (pg: String) (pred: Pred): Option<Substitution> = 
            if pred.IsEmpty then None
            else 
              let clause = freshClause(n, pred.Head)
              let h = clause.Pred
              let b = clause.Args
              match mostGeneralUnifier(Var pg, h, s) with
              | None -> tryEval s gs n pg pred.Tail
              | Some(s0) -> this.Eval(s0, List.append b gs, n + 1).GetOrElse(tryEval s gs n pg pred.Tail)
          
          let name = sprintf "TOP%d" depth
          let subToTry = sub.Update(Atom name, app)
          this.PredicateResult(op) |> Result.map(tryEval subToTry query.Tail depth name)
        | App(_) -> Error(sprintf "Only atoms can identify predicates currently.")
    
    member this.Ask(q: Query): Answer = this.Eval(sub, q, 0)
  
  type Repl(prompt: String, env: Prolog) = 
    member this.Assert(name: String, pred: Pred) = env.DB(name) <- pred
    member this.Ask(query: Query): Answer = env.Ask(query)
  
  [<EntryPoint>]
  let main args = 0
