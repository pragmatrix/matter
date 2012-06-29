module Runtime

open Expression

let essentialFunctions = [
    "list", List
    "head", fun (e::_) -> e
    "tail", fun (_::t) -> List t
    ]

let functionMap = 
    let conv (name, f) =
        name, Lambda f

    let pairs = List.map conv essentialFunctions
    Map.ofList pairs
