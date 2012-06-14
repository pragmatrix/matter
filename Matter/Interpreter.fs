module Interpreter

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
    doify (parseString content)

let staticMatter = loadMatter()

let interpretString str =
    let prelude = staticMatter
    let expressions = parseString str
    let program = doify (prelude :: expressions)
    eval program Map.empty |> fst
