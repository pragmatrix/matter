module ScopingTests

open NUnit.Framework

open Syntax
open Interpreter
open Expression

[<TestFixture>]
type ScopingTests() =

    let test l r = 
        Assert.That(print l, Is.EqualTo(print r))

    let interpretString = interpretString indentSyntax

    [<Test>]
    member this.testVisibleBelow() =
        let str = "
            def a 10
            def b (if true a 11)
            b
            "
        let r = interpretString str
        test r (Number 10)
