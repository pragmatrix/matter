// Learn more about F# at http://fsharp.net

module Module1

open Interpreter
open Syntax
open Expression
open System

open NUnit.Framework

[<TestFixture>]
type TokenizerTests() = 
    
    [<Test>]
    member this.testTokenizer() =
        this.test ("take-while (= 4) '(4 4 5 6)", "(4 4)")

    member this.test(str, expected) =
        let prefix = "use \"tokenizer.mt\"\n"
        let r = interpretString indentSyntax (prefix + str)
        let printed = print r
        if (expected <> printed) then
            raise (Exception(printed))


            
           




