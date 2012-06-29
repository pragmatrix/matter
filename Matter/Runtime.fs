module Runtime

open Expression

let runtimeFunctions = [
    "list", List
    "head", fun (e::_) -> e
    "tail", fun (_::t) -> List t
    ]

let functionMap = 
    let conv (name, f) =
        name, Lambda f

    let pairs = List.map conv runtimeFunctions
    Map.ofList pairs
