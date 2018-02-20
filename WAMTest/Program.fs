// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace WAM

module Program =
    open System
    open System.Threading

    [<EntryPoint>]
    let main argv = 
        printfn("HELLO TESTS")
        let mutable n = 0
        while (n < 1000000) do n <- n + 1
        0
