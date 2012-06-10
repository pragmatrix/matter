module EvaluatorTests

open NUnit.Framework

open Parser
open Evaluator

[<TestFixture>]
type EvaluatorTests() =

    [<Test>]
    member this.testLiteral() =
        let r = evaluateString "10"
        Assert.That(r, Is.EqualTo(Number 10))

    [<Test>]
    member this.testSymbol() =
        let r = evaluateString "(define a 10) a"
        Assert.That(r, Is.EqualTo(Number 10))
    
    [<Test>]
    member this.testFunction() =
        let r = evaluateString "(define first (a b) a) (first 10 11)"
        Assert.That(r, Is.EqualTo(Number 10))

    [<Test>]
    member this.testFunction2() =
        let r = evaluateString "(define second (a b) b) (second 10 11)"
        Assert.That(r, Is.EqualTo(Number 11))

    [<Test>]
    member this.testIf() =
        let r = evaluateString "(define a true) (if a 11 12)"
        Assert.That(r, Is.EqualTo(Number 11))

    [<Test>]
    member this.testIf2() =
        let r = evaluateString "(define a false) (if a 11 12)"
        Assert.That(r, Is.EqualTo(Number 12))

    [<Test>]
    member this.testIfNoElse() =
        let r = evaluateString "(define a true) (if a 11)"
        Assert.That(r, Is.EqualTo(Number 11))

    [<Test>]
    member this.testIfNoElse2() =
        let r = evaluateString "(define a false) (if a 11)"
        Assert.That(r, Is.EqualTo(List []))
        