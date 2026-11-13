# Inhale - A lazyish typed interpreted language
[github link](https://github.com/carterz30cal/cs-plpdi-language)

hash: (latest, will provide a concrete hash in final report)

## Description
Inhale is a blend between functional and imperative languages that takes heavy inspiration from Haskell, whilst providing C-style syntax. Inhale intends to provide pure functions by ensuring arguments to functions are immutable.
Inhale features basic literal types, type constructors, functions, partially applied functions and a rudimentary form of input/output. Programs may be written in a style close to C, or a style close to Haskell, depending on user preference, and may be mix and matched at any time.
It has pattern matching! I was very happy with that, as it's probably my favourite feature of Haskell!

The language has several gaps in it's functionality, and a couple limitations. It does not feature while loops and lacks much in the way of syntactic sugar, although we do have the +=, -= family of the syntactic sugars available. It does allow for generics, although they may not be constrained by type (yet!). A Haskell-style prelude is included in the `examples` folder, to show how a couple basic data structures may be formed using type constructors and functions. Another limitation is that the language *guesses* the types of expressions, which forms only a very loose bound in most cases which may limit how expressive it can be. The language is also currently missing any form of string manipulation, so it's limited to using them as just literal labels - this is due to an initial design decision which has made operations too constrained in what type they may be, I might fix this in the future.

Variables are statically typed, but you can approximate dynamic typing by just assigning the variable as the `any` type, which accepts any other type. You could consider the `any` type as the base type for all subtypes, like `num` is for `int` and `float`, or alternatively you could consider it to the be the equivalent of the `Object` base class in Java. Variables may also be hidden by same-name variables in deeper scopes.

There is no built-in error handling, beyond type mismatch and emission failure, which barely qualifies in my opinion. This makes debugging rather difficult!

## Syntax
A complete description of the language's syntax may be found in the file syntax.txt, but in general statements are semicolon-separated, functions are called using round brackets and constructors are formed using square brackets.
Local variables are defined using the `let` keyword and a type, whereas functions use the keyword `function` instead, with the return type stated before the keyword. 
Outputting data happens using the `emit` keyword, like this: `emit "label" (1 + 1);`. This may output literals, constructed types and partial functions.

*I haven't got round to the operational semantics yet, i spent too long working on the interpreter and parser.*

## Implementation Details
The interpreter is written in Haskell, which seemed like a natural fit given both the similar style of the language I wanted to make and also how easily it works for making ASTs. I also found Cabal very nice to work with when developing the parser!

Within the interpreter implementation, we have two main types: `Expr` and `Statement`. `Expr` essentially holds all manner of expression with a resulting value, which could be a literal, a variable, a constructed type, a function call, anything really. `Statement`, on the other hand, is the traditional imperative command - it executes and returns a new `Context`, which in my language holds the variables, the immutable arguments, the types, and the constructors.
When combined together, `Expr` and `Statement` are all that the interpreter needs to create a manner of reasonably expressive programs. 

Now the parser, which is also written in Haskell but uses a library to handle the heavy details, is what converts a `String` into a `Sequence`, which is essentially just a list of `Statement`s. This is a mostly context-free parser, which is why the syntax is a little clunky as there cannot be ambiguity within the types and my initial design decisions made constructors and functions different objects (that's the first thing i'd change in a version 2!)

## Example Code
Please see program.otwo for an implementation of several common functions used in typical functional-style programs, as well as a basic implementation of a 'generic' list (doesn't meet some criteria as the list can contain a multitude of types at once). 

## AI
This project was made with some AI usage! The syntax was written entirely by hand, with some AI assistance in discovering typos and inconsistencies. 

The interpreter has some AI involvement, as I got further into the insanity of monads I found myself debugging with GPT-6 a fair bit. However the code was still majority written by myself, besides some smaller helper functions.

The parser had the most involvement, as it also delves into monad use a fair bit and I'll admit I didn't have the best understanding of this section of Haskell, as well as using an entirely new library for the first time! 

The preprocessor/loader was mostly AI generated, as it's not at all required for the coursework and I just thought it'd be fun.

The report is entirely hand-written, and hasn't touched an AI. 

## Collaboration
I did not discuss my project with any current PLPDI students. I did, however, send a very excited message to several current & former students when my parser worked for the first time and then a further message when I thought it was bugged when it actually turned out that I had just coded the fibonacci sequence in my language wrong. 





