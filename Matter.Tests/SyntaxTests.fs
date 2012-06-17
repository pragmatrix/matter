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
