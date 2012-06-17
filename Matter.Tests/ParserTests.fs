module ParserTests

open NUnit.Framework

open Syntax
open Parser
open Expression

[<TestFixture>]
type ParserTests() =

    let test l r = 
        Assert.That(print (List l), Is.EqualTo(print (List r)))

    let parseString = parseString braceSyntax

    [<Test>]
    member this.testEmpty() =
        let res = parseString ""
        test res []

    [<Test>]
    member this.testEmptyList() =
        let res = parseString "()"
        test res [List []]

    [<Test>]
    member this.testAtom() =
        let res = parseString "10"
        test res [Number 10]

    [<Test>]
    member this.testAtomInList() =
        let res = parseString "(10)"
        test res [List [Number 10]]

    [<Test>]
    member this.testAtomsInList() =
        let res = parseString "(10 11)"
        test res [List [Number 10; Number 11]]

    [<Test>]
    member this.testListInList() =
        let res = parseString "(())"
        test res [List [List []]]

    [<Test>]
    member this.testAtomSequence() =
        let res = parseString "10 11"
        test res [Number 10; Number 11]

    [<Test>]
    member this.testListSequence() =
        let res = parseString "()()"
        test res [List[];List[]]

    [<Test>]
    member this.testQuoteAtom() = 
        let res = parseString "10 101 '11 12 13"
        test res [Number 10; Number 101; List[Symbol "quote"; Number 11]; Number 12; Number 13]

    [<Test>]
    member this.testQuoteList() =
        let res = parseString "'(10 11) (9)"
        test res [List [Symbol "quote"; List[Number 10; Number 11]]; List[Number 9]]

