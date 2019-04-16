# tarot-compiler

Tarot is a scheme compiler written in scheme that runs on top of a virtual machine written in C.

My aim with tarot was to make a scheme implementation that was capable of building itself as well as being implemented in a clear and concise way. Basically it was a learning project and there are much better and more complete implementations of scheme out there. After I completed it, I also wanted to make it bootstrappable instead of just being self hosted. I achieved this using the tinyscheme interpreter. but tinyscheme suddenly stopped building it due to a GC error, so I created my own interpreter single_cream.

# use

There are much better scheme implementations out there, so this mostly exists for reading. I hope it is interesting and people can learn something from it.

It is not very user friendly and doesn't have a REPL. You can look at the test programs to see the sort of language it implements (roughly it's a subset of R5RS without syntax rules or set!).

I have documented some internals of the compiler here: https://rain-1.github.io/scheme

The instructions on how to build and test it are here: https://rain-1.github.io/scheme-9

If you have any questions about the system I welcome you to ask.
