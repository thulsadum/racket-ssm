#lang brag

program : ( text-segment | data-segment | code-segment )*

text-segment : /SEG-TEXT ( string-def )*
string-def : LABEL STRING | STRING

data-segment : /SEG-DATA ( definition )*
@definition : initialization | allocation
initialization : EQU ( NUMBER )+ | LABEL EQU ( NUMBER )+
allocation :  RES NUMBER | RES NUMBER NUMBER |
              LABEL RES NUMBER | LABEL RES NUMBER NUMBER

code-segment : /SEG-CODE ( instruction-with-label | instruction )*
instruction-with-label : LABEL instruction
@instruction : stack-op | memory-op | arithmetic-or-logic | branching | input-output | halt

@stack-op : push | drop | dup | swap | rot
@push : push-number | push-label
push-number : /PUSH NUMBER
push-label : /PUSH LABEL
drop : /DROP
dup : /DUP
swap : /SWAP
rot : /ROT

@memory-op : load | store
load : /LOAD
store : /STORE

@arithmetic-or-logic : add | sub | mul | div | mod |
                      and | or | xor | not |
                      gt | ge | eq | le | lt
add : /ADD
sub : /SUB
mul : /MUL
div : /DIV
mod : /MOD
and : /AND
or  : /OR
xor : /XOR
not : /NOT
gt  : /GT
ge  : /GE
eq  : /EQ
le  : /LE
lt  : /LT

@branching : jump | call | return | when | unless
jump : /JUMP LABEL
call : /CALL LABEL
return : /RETURN
when : /WHEN LABEL
unless : /UNLESS LABEL

@input-output : putc | puti | puts | getc
putc : /PUTC
puti : /PUTI
puts : /PUTS
getc : /GETC


halt : /HALT