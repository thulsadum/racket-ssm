#lang scribble/manual
@require[@for-label[ssm
                    racket/base]]

@title{simple-stack-machine}
@author{basty}

@defmodule[ssm]

A simple stack machine assembly format with reference implementation in racket.

@table-of-contents[]


@section{Motivation}

@subsection{Why the simple stack machine?}



@subsection{Example}

An assembly example.

@codeblock{
 #lang ssm
 ; the text segment is used to define string literals.
 SEG TEXT
   msg "Hello World!\n"
   true-caption "The answer to the universe and everythin is: "
 ; the data segment is used to define/allocate static data.
 SEG DATA
   the-truth equ 42
   an-array  res the-truth
 ; the code segment is used to define the actual code
 SEG CODE
   push msg ;; <==> push 0
   puts ;; prints string-table[pop()]
   push true-caption ;; <==> push 1
   puts
   push the-truth ;; <==> push 0
   loadd ;; push(data[pop()])
   puti ;; prints integer representation of pop()
   push '\n' ;; pushes character literal as data word
   putc ;; prints pop() as character
}


@section{Specification}

@subsection{Assembly format}

An assembly file contains at least a @seclink["cseg"]{code segment} and may contain optional
@seclink["tseg"]{text} or @seclink["dseg"]{data segment}.

Any segment might occure multiple times. They are concatenated in order of appeareance.

@subsubsection[#:tag "dseg"]{The Data Segment}
This segment contains staticly allocated data. Labels might be omitted, if you keep track of the
memory positions (starting from zero) by yourself.

@codeblock[#:keep-lang-line? #f]{#lang ssm
SEG DATA
 label equ 42}
Allocates a word initialized to @code{42}.

@codeblock[#:keep-lang-line? #f]{#lang ssm
SEG DATA
 label equ 0 1 2 3}
Allocates four words initialized to @code{0}, @code{1}, @code{2}, @code{3}.

@codeblock[#:keep-lang-line? #f]{#lang ssm
SEG DATA
 label res 10 0}
Allocates ten words each initialized to @code{0}.


@subsubsection[#:tag "tseg"]{The Text Segment}
This section contains a string table. Labels might be omitted, if you keep track of the indexes
(starting from zero) by yourself.

@codeblock[#:keep-lang-line? #f]{#lang ssm
SEG TEXT
 label "Hello World!\n"}
Puts a string @code{"Hello World!\n"} into the string table.

@subsubsection[#:tag "cseg"]{The Code Segment}
If no segment is specified, this is the default segment. Here, the actual program is declared.

The general syntax is
@codeblock[#:keep-lang-line? #f]{#lang ssm
SEG CODE
 label: push a
    push b
    add
    push c
    jmp label}

See @secref{instructions} for details.

@subsection{The virtual machine}

@subsubsection{The internal state (aka memory)}

@subsubsection{The stack}


@subsection[#:tag "instructions"]{Instructions}

@subsubsection{Stack operations}
@codeblock[#:keep-lang-line? #f]{#lang ssm
 push 42
}
Pushes the integer literal @code{42} on top of the stack.



@codeblock[#:keep-lang-line? #f]{#lang ssm
 push 'a'
}
Pushes the character literal @code{#\a} on top of the stack. Interpreted as an integer.
So effectively the top of the stack corresponds to: @code{97}.



@codeblock[#:keep-lang-line? #f]{#lang ssm
 push a-label
}

Pushes the index of a string in the string table (aka text segment);
or the offset of a memory cell of the static memory (aka data segment);
or the offset of an instruction in the code segment onto the stack.



@codeblock[#:keep-lang-line? #f]{#lang ssm
 drop
}

Discards the top of the stack.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 dup
}

Duplicates the top value.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 swap
}

Swaps the top and the second stack value.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 rot
}

Stack rotation: [... 3 2 1] ==> [... 2 1 3].


@subsubsection{Input/Output}

@codeblock[#:keep-lang-line? #f]{#lang ssm
 putc
}

Outputs the top value interpreted as a character.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 puti
}

Outputs the top value interpreted as an integer.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 puts
}

Outputs the string pointed to by the top value.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 getc
}

Reads a character from input and pushes its integral value onto the stack.


@subsubsection{Memory access}

@codeblock[#:keep-lang-line? #f]{#lang ssm
 load
}

Loads the memory cell pointed to by the top value onto the stack.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 store
}

Stores the second value of the stack to the memory cell pointed to by the top value of the stack.


@subsubsection{Arithmetic and Logic}

@codeblock[#:keep-lang-line? #f]{#lang ssm
 add
 sub
 mul
 div
 mod
 and
 or
 xor
}

push(S1 `op` S0).

@codeblock[#:keep-lang-line? #f]{#lang ssm
 not
}

Flips every bit of the top value.


@codeblock[#:keep-lang-line? #f]{#lang ssm
 gt
 ge
 eq
 le
 lt
}

Pushes (not 0 == -1) onto the stack, iff S1 `rel` S0. Otherwise, push 0.
Used for conditional branchning.


@subsubsection{Branching}

@codeblock[#:keep-lang-line? #f]{#lang ssm
 jump label
}

Jumps unconditionally to the location pointed to by the label.

@codeblock[#:keep-lang-line? #f]{#lang ssm
 call label
}

Jumps unconditionally to the location pointed to by the label, pushing the old PC onto the stack.

@codeblock[#:keep-lang-line? #f]{#lang ssm
 return
}

Jumps to the location pointed to by the top of the stack (i.e. returning from a call).


@codeblock[#:keep-lang-line? #f]{#lang ssm
 when label
}

Jumps to the location pointed to by the label, iff the top value represents a value of true (not 0).

@codeblock[#:keep-lang-line? #f]{#lang ssm
 unless label
}

Jumps to the location pointed to by the label, iff the top value represents a value of false (0).

@subsubsection{Misc.}

@codeblock[#:keep-lang-line? #f]{#lang ssm
 halt
}

Stops the virtual machine.
