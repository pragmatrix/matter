module ParserTests

open NUnit.Framework
open Parser


[<TestFixture>]
type ParserTests() =

    [<Test>]
    member this.testEmpty() =
        let res = parseString ""
        Assert.That(res, Is.EqualTo([]))

    [<Test>]
    member this.testEmptyList() =
        let res = parseString "()"
        Assert.That(res, Is.EqualTo([List []]))

    [<Test>]
    member this.testAtom() =
        let res = parseString "10"
        Assert.That(res, Is.EqualTo([Number 10]))

    [<Test>]
    member this.testAtomInList() =
        let res = parseString "(10)"
        Assert.That(res, Is.EqualTo([List [Number 10]]))

    [<Test>]
    member this.testAtomsInList() =
        let res = parseString "(10 11)"
        Assert.That(res, Is.EqualTo([List [Number 10; Number 11]]))

    [<Test>]
    member this.testListInList() =
        let res = parseString "(())"
        Assert.That(res, Is.EqualTo([List [List []]]))

    [<Test>]
    member this.testAtomSequence() =
        let res = parseString "10 11"
        Assert.That(res, Is.EqualTo([Number 10;Number 11]))

    [<Test>]
    member this.testListSequence() =
        let res = parseString "()()"
        Assert.That(res, Is.EqualTo([List[];List[]]))
