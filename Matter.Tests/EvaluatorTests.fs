﻿module EvaluatorTests

open NUnit.Framework

open Evaluator
open Expression

[<TestFixture>]
type EvaluatorTests() =

    let test l r = 
        Assert.That(print l, Is.EqualTo(print r))

    [<Test>]
    member this.testLiteral() =
        let r = evaluateString "11"
        test r (Number 11)

    [<Test>]
    member this.testSymbol() =
        let r = evaluateString "(def a 10) a"
        test r (Number 10)
    
    [<Test>]
    member this.testFunction() =
        let r = evaluateString "(def first (a b) a) (first 10 11)"
        test r (Number 10)

    [<Test>]
    member this.testFunction2() =
        let r = evaluateString "(def second (a b) b) (second 10 11)"
        test r (Number 11)

    [<Test>]
    member this.testIf() =
        let r = evaluateString "(def a true) (if a 11 12)"
        test r (Number 11)

    [<Test>]
    member this.testIf2() =
        let r = evaluateString "(def a false) (if a 11 12)"
        test r (Number 12)

    [<Test>]
    member this.testIfNoElse() =
        let r = evaluateString "(def a true) (if a 11)"
        test r (Number 11)

    [<Test>]
    member this.testIfNoElse2() =
        let r = evaluateString "(def a false) (if a 11)"
        test r (List [])

    [<Test>]
    member this.testMacroSimple() =
        let r = evaluateString "(defmacro a (if b 1 0)) (def b true) (a)"
        test r (Number 1)
        