## Matter - A lisp inspired language in its infancy.

Matter is a clean implementation of a pure functional, dynamically typed language that targets live programming environments.

Matter is being bootstrapped using F#, but should be independent of the runtime environment. It may however require garbage collection and tail calls.

Although in its early stages, it already supports a number of features:

- [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity) -> the language is represented in its own datatypes
- macros -> metaprogramming, extend the language
- indentation syntax -> less braces
- simultaneous scoping (no declares like in clojure)

The next steps are

- bootstrapping the language -> port all F# code to Matter
- separate the analyzer from the evaluator so that the interpreter gets faster and compilation gets possible
- (optional) a language neutral compilation backend
