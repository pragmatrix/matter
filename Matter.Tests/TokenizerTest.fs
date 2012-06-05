// Learn more about F# at http://fsharp.net

module TokenizerTests

open Tokenizer
open NUnit.Framework


[<TestFixture>]
type TokenizerTests() =

    [<Test>]
    member this.testSymbol() =
        let res = tokenizeString "Hello"
        Assert.That(res, Is.EqualTo([ Symbol "Hello" ]))

    [<Test>]
    member this.testKeyword() =
        let res = tokenizeString ":Keyword"
        Assert.That(res, Is.EqualTo([Keyword "Keyword"]))

    [<Test>]
    member this.testNumber() =
        let res = tokenizeString "156"
        Assert.That(res, Is.EqualTo([Number 156] ))

    [<Test>]
    member this.testBoolean() =
        let res = tokenizeString "true"
        Assert.That(res, Is.EqualTo([Boolean true]))

        let res = tokenizeString "false"
        Assert.That(res, Is.EqualTo([Boolean false]))

    [<Test>]
    member this.testString() =
        let str = "\"Hi!\"";
        let res = tokenizeString str
        Assert.That(res, Is.EqualTo([String "Hi!"]))

    [<Test>]
    member this.testBegin() =
        let res = tokenizeString "("
        Assert.That(res, Is.EqualTo([Begin]))

    [<Test>]
    member this.testQuote() =
        let res = tokenizeString "'"
        Assert.That(res, Is.EqualTo([Quote]))

    [<Test>]
    member this.testWhitespace() =
        let res = tokenizeString "  \n\rHuhu"
        Assert.That(res, Is.EqualTo([Symbol "Huhu"]))
        let res = tokenizeString "Huhu  \n\r"
        Assert.That(res, Is.EqualTo([Symbol "Huhu"]))

    [<Test>]
    member this.testOrdering() =
        let res = tokenizeString "hoi ho"
        let expected = [Symbol "hoi"; Symbol "ho"]
        Assert.That(res, Is.EqualTo(expected))
