# Inhale - A lazyish typed interpreted language
[github link](https://github.com/carterz30cal/cs-plpdi-language)

hash: (latest, will provide a concrete hash in final report)

## Description
Inhale is a blend between functional and imperative languages that takes heavy inspiration from Haskell, whilst providing C-style syntax. Inhale intends to provide pure functions by ensuring arguments to functions are immutable.
Inhale features basic literal types, type constructors, functions, partially applied functions and a rudimentary form of input/output. Programs may be written in a style close to C, or a style close to Haskell, depending on user preference, and may be mix and matched at any time.
It has pattern matching! I was very happy with that, as it's probably my favourite feature of Haskell!

The language has several limitations. It does not feature while loops, generics and lacks much syntactic sugar, although generics may be approximated using the `any` type, to a certain degree. A Haskell-style prelude program is included to show off how basic lists may be formed using type constructors and functions. The language also *guesses* the types of most expressions, to avoid evaluation, but these types are a very loose bound in most cases (besides literals and function calls essentially) and so may not be expressive enough for some programs. Variables are either statically typed or dynamically typed if given the `any` type upon definition, which accepts any type and so is in practice dynamic. You could consider the `any` type to be like the `Object` base class in Java.
There's also no error handling really.

## Syntax
A complete description of the language's syntax may be found in the file syntax.txt, but in general statements are semicolon-separated, functions are called using round brackets and constructors are formed using square brackets.
Local variables are defined using the `let` keyword and a type, whereas functions use the keyword `function` instead, with the return type stated before the keyword. 
Outputting data happens using the `emit` keyword, like this: `emit "label" (1 + 1);`. This may output literals, constructed types and partial functions.

*I haven't got round to the operational semantics yet, i spent too long working on the interpreter and parser.*

## Implementation Details
The interpreter is written in Haskell, which seemed like a natural fit given both the similar style of the language I wanted to make and also how easily it works for making ASTs. I also found Cabal very nice to work with when developing the parser!

Within the interpreter implementation, we have two main types: `Expr` and `Statement`. `Expr` essentially holds all manner of expression with a resulting value, which could be a literal, a variable, a constructed type, a function call, anything really. `Statement`, on the other hand, is the traditional imperative command - it executes and returns a new `Context`, which in my language holds the variables, the immutable arguments, the types,and the constructors.
When combined together, `Expr` and `Statement` are all that the interpreter needs to create a manner of reasonably expressive programs. 

Now the parser, which is also written in Haskell but uses a library to handle the heavy details, is what converts a `String` into a `Sequence`, which is essentially just a list of `Statement`s. This is a mostly context-free parser, which is why the syntax is a little clunky as there cannot be ambiguity within the types and my initial design decisions made constructors and functions different objects (that's the first thing i'd change in a version 2!)

## Example Code
Please see program.lang for an implementation of several common functions used in typical functional-style programs, as well as a basic implementation of a 'generic' list (doesn't meet some criteria as the list can contain a multitude of types at once). 

## AI
This project was made with some AI usage! The syntax was written entirely by hand, with some AI assistance in discovering typos and inconsistencies. 
The interpreter has some AI involvement, as I got further into the insanity of monads I found myself debugging with GPT-6 a fair bit. However the code was still majority written by myself, besides some smaller helper functions.
The parser had the most involvement, as it also delves into monad use a fair bit and I'll admit I didn't have the best understanding of this section of Haskell, as well as using an entirely new library for the first time! 
The report is entirely hand-written, and hasn't touched an AI. 

## Collaboration
I did not discuss my project with any current PLPDI students. I did, however, send a very excited message to several current & former students when my parser worked for the first time and then a further message when I thought it was bugged when it actually turned out that I had just coded the fibonacci sequence in my language wrong. 





