module Runtime

open Expression
open Parser
open Syntax

open System.IO

let rec equalFunction exp = 
    match exp with
    // partial function application (feels like a hack here)
    | [l] -> Lambda (fun exp -> equalFunction (l::exp))
    | [l; r] -> equal l r |> Boolean
    | _ -> failwith "= expects two arguments"

let runtimeFunctions = [
    "list", List
    "first", fun (e::_) -> e
    "next", fun (_::t) -> List t
    "=", equalFunction
    ]

let useMacro [String str] =
    let content = File.ReadAllText(str)
    let parsed = parseString indentSyntax content
    doify parsed

let runtimeMacros = [
    "use", useMacro
    ]

let runtimeRecords = 
    let convFunction (name, f) =
        name, Lambda f

    let convMacro (name, f) =
        name, Expression.Macro f

    let functions = List.map convFunction runtimeFunctions
    let macros = List.map convMacro runtimeMacros
    Map.ofList (functions @ macros)
