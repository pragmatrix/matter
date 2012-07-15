module Interpreter

open Syntax
open Expression
open Parser
open Evaluator
open System.IO

open System.Reflection

let loadFromManifest (assembly:Assembly) name =
    use stream = assembly.GetManifestResourceStream(name)
    use reader = new StreamReader(stream)
    let content = reader.ReadToEnd()
    doify (parseString indentSyntax content)

let loadMatter() = loadFromManifest (Assembly.GetExecutingAssembly()) "matter.mt"

let staticMatter = loadMatter()

let interpretString syntax str =
    let prelude = staticMatter
    let expressions = parseString syntax str
    let program = doify (prelude :: expressions)
    eval program Frame.empty |> fst
