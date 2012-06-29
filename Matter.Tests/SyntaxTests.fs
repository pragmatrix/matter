module SyntaxTests

open NUnit.Framework

open Tokenizer
open Syntax

[<TestFixture>]
type SyntaxTests() =

    let test (res : Token list) (expected : string) = Assert.That(res, Is.EqualTo(tokenizeString expected))

    let run = tokenizeString >> indentSyntax

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
        let res = run "def a 10\n\n\t\t\n\ndef b 11"
        test res "(def a 10) (def b 11)"

    // a single symbol is not enclosed in parenthesis.
    [<Test>]
    member this.testSingleSymbol() =
        let res = run "a";
        test res "a";

    // likewise a literal is not
    [<Test>]
    member this.testSingleLiteral() =
        let res = run "10";
        test res "10";

    // explicit enclosing one token leaves one level of paranthesis
    [<Test>]
    member this.testSingleLiteralInParens() =
        let res = run "(10)";
        test res "(10)";

    [<Test>]
    member this.testIfThenElse() =
        let res = run "
            if true
            \tdef a 10
            \tdef a 11
            "

        test res "(if true (def a 10) (def a 11))"
