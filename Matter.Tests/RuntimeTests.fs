module RuntimeTests

open NUnit.Framework

open Runtime
open Expression

[<TestFixture>]
type RuntimeTests() =

    [<Test>]
    member this.testMatter() =
        loadMatter() |> ignore
        
