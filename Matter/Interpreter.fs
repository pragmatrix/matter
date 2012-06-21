module Interpreter

open Syntax
open Expression
open Parser
open Evaluator
open System.IO

open System.Reflection


let loadMatter() =
    let assembly = Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream("matter.mt")
    use reader = new StreamReader(stream)
    let content = reader.ReadToEnd()
    doify (parseString braceSyntax content)

let staticMatter = loadMatter()

let interpretString syntax str =
    let prelude = staticMatter
    let expressions = parseString syntax str
    let program = doify (prelude :: expressions)
    eval program Frame.empty |> fst
