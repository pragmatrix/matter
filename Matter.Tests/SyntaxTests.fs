module SyntaxTests

open NUnit.Framework

open Tokenizer
open Syntax

[<TestFixture>]
type SyntaxTests() =

    let test (res : Token list) (expected : string) = Assert.That(res, Is.EqualTo(tokenizeString expected))

    let run = tokenizeString >> lightSyntax

    [<Test>]
    member this.testSingleLine() = 
        let res = run "def test 1"
        test res "(def test 1)"

    [<Test>]
    member this.testSingleLineIndented() =
        let res = run "\tdef test 1"
        test res "(def test 1)"

    [<Test>]
    member this.testSurroundingNewlines() =
        let res = run "\n\tdef test 1\n"
        test res "(def test 1)"

    [<Test>]
    member this.testIndent() =
        let res = run "def test 1\n\t+ 10 10"
        test res "(def test 1 (+ 10 10))"

    [<Test>]
    member this.testIndentOutdent() =
        let res = run "def test 1\n\t+ 10 10\ndef test2"
        test res "(def test 1 (+ 10 10))(def test2)"

    [<Test>]
    member this.testLineCrossing() =
        let res = run "def test 1\n, 10"
        test res "(def test 1 10)"

    [<Test>]
    member this.testEmptyLinesWithIndent() =
        let res = run "def\n\n\t\t\n\ndef2"
        test res "(def) (def2)"
