# An Unlisp Toy Compiler

This is an unlisp-to-machine code compiler, which produces a fully working static ELF64 executable by default (and it is written in a way that alternative binary formats may be swapped rather easily in principle).
The generated program may be no more than 65536 bytes long and the data segment is no more than 65536 bytes also: it was designed as a level-0 bootstrap.

It is a Wirth-inspired single-pass compiler for a lisp-like LL(1) language (with several limitations such as max 8 parameters per function; untyped, fully boxed data; and lacking most error checking).
The source is in x86\_64 assembly language targeting linux (with the platform-specific parts sequestrated to different source files), and the make.sh script shows an example gcc invocation to assemble the compiler.
There is no use of C or of external libraries whatsoever in this project.

It was created as a learning project to better understand a theoretical bootstrap sequence from assembly language up: how small can a practical (for some definition) language be? Which parts are trickier or easier? Which parts take up more code (in development time and in instructions)?
In addition, it was a way to learn about ELF and machine code and to get more familiar with x86\_64.

Provided here for those who might grasp something from it. Note that the code quality is very low, this is not a good example of an x86\_64 codebase.

## Example

```
; Comments are started with a single ; and continue to the end of the line
(def x "hello\n") ;; definition
(def y "hi\n")
(def str "\tIt works!\n") ;; strings support various escapes
(def print-it (fn (y) (print y))) ;; functions
(def switch-print (fn (b x y)
	(if b
		(print x)
		(print-it y)))) ;; conditionals
(def print-times
	(fn (cnt x)
		(if cnt
			((fn (a) (print-times (dec cnt) x)) (print x)) ;; sequencing and recursion
			(nop))))
(print-it x) ;; calling functions
(switch-print 1 x y)
(switch-print 0 y x)
(switch-print 3 y x)
(print-times 5 str)
```

Should output:

```
hello
hello
hello
hi
	It works!
	It works!
	It works!
	It works!
	It works!
```

## Methods

Parsing proceeds by recursive descent. Register allocation is done in the naive way: parameters are mov'd into registers in a predefined order and wastefully spilled/filled around function calls. All data are 64-bits.
The implementation is deeply flawed in many ways, but it's a fun proof of concept.

A valid program takes the following form:

```
program = form {form}
form = "(" expr {expr} ")"
expr = atom | form
atom = literal | var
var = _char | _symb {_char | _symb | _num}
literal = string | int
string = "\"" _any "\""
int = _num

_char ~ [a-zA-Z]
_symb ~ [^a-zA-Z0-9\s]
_num ~ [0-9]
_any ~ \S
```

All input is ascii and objects are separated by arbitrarily many whitespace characters (EOT (ascii 3) does not count as whitespace for this purpose).
For a more accurate depiction of the language recognized by the compiler, see the compiler's source.
Data is untyped and program validity check is only summary.

The recognized initial constructs are:

```
(def symb val): defines the symbol symb to take the value val.
(f), (f x), (f x ... y): call function f on all provided arguments (can be none, and no more than 8).
(if a b c): if a is non-0, b otherwise c based on the rules in this paragraph.
(fn () x), (fn (a) x), (fn (a ... b) x): declare a function whose parameters are within the parentheses after the \\, and whose call-time effect is x, as per the rules in this paragraph.
(nop): do nothing.
```

The initial functions available in the program are:

```
(print x): outputs the string x to stdout.
(not x): boolean not.
(dec x): returns x-1.
(inc x): returns x+1.
(> x y): 1 if x > y else 0.
(< x y): 1 if x < y else 0.
```

Strings support some escape sequences:

```
\0 for NUL.
\n for newline.
\t for tab.
\\ for literal backslash character.
\" for literal quote character.
```

## Conclusions

### x86\_64

- Use of arrays and what amounts to structs is instrumental to reducing total code size. For example, it would be better to have taken the special forms (def, nop, if, \\) and to have put them in an array, then loop on it to match the forms, rather than the current
implementation, which uses a "flattened loop".
- Alignment of the data also impacts code size: symbol\_tbl management needs 2 instructions to compute the real index because of its 3-bytes alignment, whereas a single instruction is needed for 1, 2, 4, or 8-byte alignments.
- Surprisingly many things can be done without any dynamic memory management, although it also involves wasted memory.
- Due to rep* mov*/cmp*, terminated-strings (e.g. 0-terminated) are somewhat more code-efficient than other formats.

### Compilers and Languages

- Correct generation of the binary is complicated foremost by the binary format, not by the actual constructs supported, due mostly to very poor documentation. This is probably no longer the case when more realistic register management is implemented.
- Of the constructs supported, functions (i.e. fn) are the most complicated, other constructs are straightforward to implement naively.
- Generated program register management appears to be the single largest complication in the entire process. A simple and wasteful scheme was implemented here, while full graph coloring-based approaches may require significant code.
- The bootstrap should probably focus on implementation simplicity: while here we pass everything on registers by default, a bootstrap compiler should probably just pass everything on the stack or in dedicated memory regions or the like to greatly simplify the code.
- Managing whitespace has a non-negligible effect on code size, i.e. the forth syntax has an implementation advantage and such a syntactic constraint might make sense for the lowest-level bootstrap.
- Despite the few constructs this language is made of, it is surprisingly powerful, suggesting that bootstrapping as fast as possible (for example with an interpreter instead of a compiler) might be greatly beneficial to the process.
- It could be advantageous to make use of language constructs as the compiler implementation progresses. For example, put the code for str-\>int in the program(%rip), and then `call` it in the compiler when we need that same function, e.g. when reading int from input, instead of implementing
	basically twice (once as x86\_64 and once as machine code). As a bonus, it is a kind of test on the generated program.
	This also has a few additional advantages: it should make implementing all the primitives used in the x86\_64 program straightforward or even automatic, allowing us to provide read, compile and (at least parts of) print to the language with little to no extra code.
- While macros were deliberately avoided in this project, in real life they would have cut down quite a lot of code. However, foregoing macros allows implementing an assembler in machine code far more easily if that is ever a concern. While this isn't considered at this point, this could be an interesting
	far future endeavor if an interesting chip shows up without an assembler available (I believe this applies for example to seaForth and green array chips, which use Forth instead "as" the assembly language).
- The true-1-pass-with-recursive-decent approach is a neat trick, but it results in running unnecessary code-paths. The 1-pass parsing does not seem to have any issue, but code generation is more complicated.

### General

- I used several hacks to work around limitations of the syntax description from above: it would have been more productive to speciate the syntax better instead, it ended up adding a lot of otherwise unncessary code.
- A major stylistic misstep is the gross lack of factorization in the code. Maybe 20% of the code could be removed with just half an hour's worth of refactoring.

The conclusions are mostly encouraging. For the next steps, it would be a good idea to use a higher-level language and focus on language design rather than compiler implementation, lastly putting it all together.
