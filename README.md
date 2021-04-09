# An Unlisp Toy Compiler

This is an unlisp-to-machine code compiler, which produces a fully working static ELF64 executable by default (and it is written in a way that alternative binary formats may be swapped rather easily in principle), so long as
the program is no more than 65536 bytes long and the data segment is no more than 65536 bytes also.

It is a Wirth-inspired single-pass compiler for a lisp-like LL(1) language (with several limitations such as no expression in function position; max 8 parameters per function; untyped, fully boxed data; and lacking error checking).
The source is in x86\_64 assembly language targetting linux (with the platform-specific parts sequestrated to different source files), and the make.sh script shows an example gcc invokation to assemble the compiler.
There is no use of C or of external libraries whatsoever in this project.

It was created as a learning project to better understand a theoretical bootstrap sequence from assembly language up: how small can a practical (for some definition) language be? Which parts are trickier or easier? Which parts take up more code (in development time and in instructions)?
In addition, it was a way to learn about ELF and machine code.

In principle, only more primitives need be added to make it capable of self-hosting. This is left as an exercise to the reader ;)

Provided here for those who might grasp something from it.

Example

```
(def x "hello\n")
(def y "hi\n")
(def str "\tIt works!\n")
(def print-it (\ y (print y)))
(def switch-print (\ b x y
	(if b
		(print x)
		(print-it y))))
(def print-times (\ cnt x
					(if cnt (do (print x) (print-times (1- cnt) x))
							(nop))))
(print-it x)
(switch-print 1 x y)
(switch-print 0 y x)
(switch-print 3 x y)
(print-times 5 str)
```

Should output:

```
hello
hi
hi
hi
	It works!
	It works!
	It works!
	It works!
	It works!
```
