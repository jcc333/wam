namespace Util

[<AutoOpen>]
module Util = 
  [<AutoOpen>]
  module List = 
    let headOption (lst: List<'a>) = if lst.IsEmpty then None else Some(lst.Head)

    type List<'a> with
      member this.HeadOption: Option<'a> = headOption this
  
  [<AutoOpen>]
  module Result = 
    type Result<'Err, 'Value> = 
      | Error of 'Err
      | Success of 'Value
  
  [<AutoOpen>]
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Prelude = 
    let inline always value = fun () -> value
    let inline always' value = fun _ -> value
  
  [<AutoOpen>]
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ResultPrelude =
    let guard f formatError =
      try
        Success(f())
      with e -> Error(formatError e)
    
    module Result =
      let inline substitute (forError: 'Err -> 'Outcome) (forSuccess: 'Value -> 'Outcome) (result: Result<'Err, 'Value>) =
        match result with
        | Error err -> forError err
        | Success value -> forSuccess value
      
      let andThen f (result: Result<'Err, 'ValueA>): Result<'Err, 'ValueB> = result |> substitute Error f
      let bind = andThen
      
      let andGuard f formatError (result: Result<'Err, 'ValueA>): Result<'Err, 'ValueB> =
        let handler value =
          try
            Success(f value)
          with e -> Error(formatError e)
        result |> andThen handler
      
      let andTry f formatError finally' (result: Result<'Err, 'ValueA>): Result<'Err, 'ValueB> = 
        let handler value = 
          try
            try
              Success(f value)
            with e -> Error(formatError e)
          finally
            finally' value
        result |> andThen handler
      
      let andError f (result: Result<'ErrA, 'Value>): Result<'ErrB, 'Value> = result |> substitute f Success
      let map f (result: Result<'Err, 'A>): Result<'Err, 'B> = result |> andThen(f >> Success)
      let formatError f (result: Result<'ErrA, 'Value>): Result<'ErrB, 'Value> = result |> andError(f >> Error)
      let isError result = result |> substitute (always' true) (always' false)
      let isSuccess result = result |> substitute (always' false) (always' true)
      let withDefault def (result: Result<'Err, 'Value>) = result |> substitute (always' def) id
      let asError(result: Result<'Err, 'Value>) = result |> substitute Some (always' None)
      let asSuccess(result: Result<'Err, 'Value>) = result |> substitute (always' None) Some
      let toOption(result: Result<'Err, 'Value>) = asSuccess result
      let iter f (result: Result<'Err, 'Value>) = result |> substitute (always'()) f
      let sinkError f (result: Result<'Err, 'Value>) = result |> substitute f (always'())
      let ret v = Result.Success(v)
      let app(fr: Result<'Err, 'A -> 'B>, vr: Result<'Err, 'A>): Result<'Err, 'B> = fr |> bind(fun f -> vr |> map f)
      let (<!>) = map
      let (<*>) = app
  
  type Result<'err, 'a> with
    member this.AndThen f = Result.andThen f this
    member this.AndTry f formatError finally' = Result.andTry f formatError finally' this
    member this.AndGuard f formatError = Result.andGuard f formatError this
    member this.Bind f = Result.bind f this
    member this.Map f = Result.map f this
    
    member inline this.IsSuccess = 
      this |> function 
      | Success(_) -> true
      | Error(_) -> false
    
    member inline this.IsFailure = not this.IsSuccess
    
    member this.OrElse(that: Result<'err, 'a>) = 
      if this.IsSuccess then this
      else that
    
    member this.GetOrElse(a: 'a): 'a = 
      this |> function 
      | Success(a) -> a
      | Error(_) -> a
  
  type Option<'a> with
    
    member this.ToResult<'e>(error: 'e): Result<'e, 'a> = 
      if this.IsNone then Error error
      else Success this.Value
    
    member this.OrElse(that: Option<'a>): Option<'a> = 
      if this.IsSome then this
      else that
    
    member this.GetOrElse(orElse: 'a) = 
      if this.IsSome then this.Value
      else orElse
    
    member this.Bind f = Option.bind f this
    member this.Map f = Option.map f this
  
  [<AutoOpen>]
  module Option = 
    let getOrElse (v: 'a) (o: Option<'a>): 'a = o.GetOrElse(v)
    let orElse (b: Option<'a>) (a: Option<'a>): Option<'a> = a.OrElse(b)
    let toResult (e: 'e) (o: Option<'a>): Result<'e, 'a> = o.ToResult(e)
