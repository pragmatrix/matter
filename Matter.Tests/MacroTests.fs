﻿module MacroTests

open NUnit.Framework

open Syntax
open Interpreter
open Expression

[<TestFixture>]
type MacroTests() =

    let test l r = 
        Assert.That(print l, Is.EqualTo(print r))

    let interpretString = interpretString braceSyntax

    [<Test>]
    member this.testMacroSimple() =
        let r = interpretString "(defmacro a '(if b 1 0)) (def b true) (a)"
        test r (Number 1)
        
    [<Test>]
    member this.testMacroParms() =
        let r = interpretString "(defmacro a (b v1 v2) (list 'if b v1 v2)) (a true 10 11)"
        test r (Number 10)

    // could we implement syntax quoting with another macro?
    // say like
    // (` if ~b ~v1 ~v2)
    // splicing:
    // (` if ~@b)
    // if yes, we shouldn't put this on the reader

    // [<Test>]
    member this.testSyntaxQuote() =
        let r = interpretString "(defmacro a (b v1 v2) `(if ~b ~v1 ~v2)) (a true 10 11)"
        test r (Number 10)
