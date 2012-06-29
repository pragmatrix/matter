﻿module ScopingTests

open NUnit.Framework

open Syntax
open Interpreter
open Expression

[<TestFixture>]
type ScopingTests() =

    let test l r = 
        Assert.That(print l, Is.EqualTo(print r))

    let interpret = interpretString indentSyntax

    [<Test>]
    member this.testVisibleBelow() =
        let str = "
            def a 10
            def b (if true a 11)
            b
            "
        let r = interpret str
        test r (Number 10)
    
    [<Test>]
    member this.testVisibleAbove() =
        let str = "
            def b (if true a 11)
            def a 10
            b
            "
        let r = interpret str
        test r (Number 10)

    [<Test>]
    member this.testEvaluationAtTop() =
        let str = "
            a
            def a 10
            "
        let r = interpret str
        test r (Number 10)

    [<Test>]
    member this.testDefInIf() =
        let str = "
            if true
            \tdef a 1
            \tdef a 2
            a
            "
        let r = interpret str
        test r (Number 1)

    [<Test>]
    member this.testSimpleLet() =
        let str = "
            let a true
            let b 1
            let c 2
            let r (if a c b)
            r
            "
        let r = interpret str
        test r (Number 2)

    [<Test>]
    member this.testLetOverwrite() =
        let str = "
            let a 1
            let a 2
            a
            "
        let r = interpret str
        test r (Number 2)
