module InterpreterTests

open NUnit.Framework

open Interpreter
open Expression

[<TestFixture>]
type InterpreterTests() =

    [<Test>]
    member this.testMatter() =
        loadMatter() |> ignore
        
