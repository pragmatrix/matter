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
        let r = evaluateString "(define (first a b) a) (first 10 11)"
        Assert.That(r, Is.EqualTo(Number 10))

    [<Test>]
    member this.testFunction2() =
        let r = evaluateString "(define (second a b) b) (second 10 11)"
        Assert.That(r, Is.EqualTo(Number 11))


