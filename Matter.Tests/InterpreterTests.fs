module InterpreterTests

open NUnit.Framework

open Interpreter
open Expression

[<TestFixture>]
type RuntimeTests() =

    [<Test>]
    member this.testMatter() =
        loadMatter() |> ignore
        
