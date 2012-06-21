## Matter - A lisp inspired language in its infancy.

Matter is a clean implementation of a pure functional, dynamically typed language that targets live programming environments.

Matter is being bootstrapped using F#, but should be independent of the runtime environment. It may however require garbage collection and tail calls.

Although in its early stages, it already supports a number of features:

- [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity) -> the language is represented in its own datatypes
- macros -> metaprogramming feature to extend the language
- indentation syntax -> less braces
- simultaneous scoping (no forward declarations like in clojure)

The next steps are

- bootstrapping the language -> port all F# code to Matter
- separate the analyzer from the evaluator so that the interpreter gets faster and compilation gets possible
- (optional) a language neutral compilation backend

## License

Copyright (c) 2012, Armin Sander
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, 
      this list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
- Neither the name of Armin Snader nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL ARMIN SANDER BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.