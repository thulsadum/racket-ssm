#lang racket

(require brag/support
         br-parser-tools/lex
         br-parser-tools/lex-sre)


(module+ test
  (require rackunit)

  (define-syntax-rule (t STR)
    (map (λ (it) (let ([the-token (srcloc-token-token it)])
                   (cons (token-name the-token) (token-value the-token))))
         (apply-tokenizer-maker make-tokenizer STR)))
  
  (define-syntax-rule (tn STR)
    (map (λ (it) (let ([the-token (srcloc-token-token it)])
                   (token-name the-token)))
         (apply-tokenizer-maker make-tokenizer STR))))


(define-empty-tokens segments (SEG-DATA SEG-TEXT SEG-CODE))
(define-empty-tokens data-ops (EQU RES))
(define-empty-tokens stack-code-ops (PUSH DROP SWAP DUP ROT))
(define-empty-tokens memory-code-ops (LOAD STORE))
(define-empty-tokens arithmetic-code-ops (ADD SUB MUL DIV MOD))
(define-empty-tokens logic-code-ops (AND OR XOR NOT GT GE EQ LE LT))
(define-empty-tokens branching-code-ops (JUMP CALL RETURN WHEN UNLESS))
(define-empty-tokens io-code-ops (PUTC PUTI PUTS GETC HALT))
(define-tokens literals (LABEL NUMBER STRING))


(module+ test
  ; check tokens
  (check-equal? (tn "SEG DATA\nSEG TEXT\nSEG CODE\n")
                '(SEG-DATA SEG-TEXT SEG-CODE))
  (check-equal? (tn "equ res")
                '(EQU RES))
  (check-equal? (tn "push drop swap dup rot")
                '(PUSH DROP SWAP DUP ROT))
  (check-equal? (tn "load store")
                '(LOAD STORE))
  (check-equal? (tn "add sub mul div mod")
                '(ADD SUB MUL DIV MOD))
  (check-equal? (tn "and or xor not gt ge eq le lt")
                '(AND OR XOR NOT GT GE EQ LE LT))
  (check-equal? (tn "jump call return when unless")
                '(JUMP CALL RETURN WHEN UNLESS))
  (check-equal? (tn "putc puti puts getc")
                '(PUTC PUTI PUTS GETC)))

(module+ test
  ; ignore comments and whitespaces
  (check-equal? (tn (string-append
                     "#lang ssm\n; empty file\n"
                     "#lang ssm\n\n #whoa, concatenation of an other empty file"))
                empty))

(module+ test
  ; literals and labels
  (check-equal? (t "42 .loop-label \"Hello\" \"Say \\\"What?\\\"\" ")
                '((NUMBER . 42)
                  (LABEL . ".loop-label")
                  (STRING . "Hello")
                  (STRING . "Say \"What?\""))))


(module+ test
  ; hello world with string
  (check-equal? (tn (string-append "SEG DATA\n"
                                   "msg \"Hello World!\"\n"
                                   "SEG CODE\n"
                                   "push msg\n"
                                   "puts\n"
                                   "halt\n"))
                '(SEG-DATA
                  LABEL STRING
                  SEG-CODE
                  PUSH LABEL
                  PUTS
                  HALT)))

(define-lex-abbrev identifier (:: (:or alphabetic punctuation)
                                  (:* (:or alphabetic numeric punctuation))))

(define ssm-lexer
  (lexer-srcloc
   [(eof) eof]
   [(:+ whitespace) (return-without-srcloc (ssm-lexer input-port))]
   [(:: (:or #\; #\#) (:* (:~ #\newline)))
    ; strip comments
    ; comments begin with ';' or '#' and extend to the end of line.
    ; # comments are a feature to ignore shebangs or #lang lines
    ; this makes "linking" files together easy. `cat source1.ssm ... sourceN.ssm` suffices.
    (return-without-srcloc (ssm-lexer input-port))]
   ["SEG DATA" (token-SEG-DATA)]
   ["SEG TEXT" (token-SEG-TEXT)]
   ["SEG CODE" (token-SEG-CODE)]
   ["equ" (token-EQU)]
   ["res" (token-RES)]
   ["push" (token-PUSH)]
   ["drop" (token-DROP)]
   ["swap" (token-SWAP)]
   ["dup" (token-DUP)]
   ["rot" (token-ROT)]
   ["load" (token-LOAD)]
   ["store" (token-STORE)]
   ["add" (token-ADD)]
   ["sub" (token-SUB)]
   ["mul" (token-MUL)]
   ["div" (token-DIV)]
   ["mod" (token-MOD)]
   ["and" (token-AND)]
   ["or" (token-OR)]
   ["xor" (token-XOR)]
   ["not" (token-NOT)]
   ["gt" (token-GT)]
   ["ge" (token-GE)]
   ["eq" (token-EQ)]
   ["le" (token-LE)]
   ["lt" (token-LT)]
   ["jump" (token-JUMP)]
   ["return" (token-RETURN)]
   ["call" (token-CALL)]
   ["when" (token-WHEN)]
   ["unless" (token-UNLESS)]
   ["putc" (token-PUTC)]
   ["puti" (token-PUTI)]
   ["puts" (token-PUTS)]
   ["getc" (token-GETC)]
   ["halt" (token-HALT)]
   ;; TODO add hex/oct/bin representation of numbers?
   [(:+ numeric) (token-NUMBER (read (open-input-string lexeme)))]
   ["'\\t'" (token-NUMBER 9)]
   ["'\\n'" (token-NUMBER 10)]
   ["'\\f'" (token-NUMBER 12)]
   ["'\\r'" (token-NUMBER 13)]
   [(from/to #\' #\') (token-NUMBER
                       (char->integer
                        (string-ref lexeme 1)))]
   [(from/to #\" (:: (:~ #\\) #\"))
    (token-STRING
     (string-replace (trim-ends "\"" lexeme "\"")
                     "\\\"" "\""))]
   [identifier (token-LABEL lexeme)]))

(define (make-tokenizer in)
  (port-count-lines! in)
  (λ () (ssm-lexer in)))
(provide make-tokenizer)