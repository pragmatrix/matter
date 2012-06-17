module EvaluatorTests

open NUnit.Framework

open Evaluator
open Expression
open Syntax

[<TestFixture>]
type EvaluatorTests() =

    let test (l, _) r = 
        Assert.That(print l, Is.EqualTo(print r))

    let evalString = evalString braceSyntax

    [<Test>]
    member this.testLiteral() =
        let r = evalString "11"
        test r (Number 11)

    [<Test>]
    member this.testSymbol() =
        let r = evalString "(def a 10) a"
        test r (Number 10)
    
    [<Test>]
    member this.testFunction() =
        let r = evalString "(def first (a b) a) (first 10 11)"
        test r (Number 10)

    [<Test>]
    member this.testFunction2() =
        let r = evalString "(def second (a b) b) (second 10 11)"
        test r (Number 11)

    [<Test>]
    member this.testIf() =
        let r = evalString "(def a true) (if a 11 12)"
        test r (Number 11)

    [<Test>]
    member this.testIf2() =
        let r = evalString "(def a false) (if a 11 12)"
        test r (Number 12)

    [<Test>]
    member this.testIfNoElse() =
        let r = evalString "(def a true) (if a 11)"
        test r (Number 11)

    [<Test>]
    member this.testIfNoElse2() =
        let r = evalString "(def a false) (if a 11)"
        test r (List [])
