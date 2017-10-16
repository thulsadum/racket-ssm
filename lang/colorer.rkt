#lang racket

(require brag/support
         br-parser-tools/lex
         br-parser-tools/lex-sre)

(define-lex-abbrev identifier (:: (:or alphabetic punctuation)
                                  (:* (:or alphabetic numeric punctuation))))

(define ssm-color-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [(:: (:or #\; #\#) (:* (:~ #\newline)))
    ; strip comments
    ; comments begin with ';' or '#' and extend to the end of line.
    ; # comments are a feature to ignore shebangs or #lang lines
    ; this makes "linking" files together easy. `cat source1.ssm ... sourceN.ssm` suffices.
    (values lexeme 'comment #f (pos lexeme-start) (pos lexeme-end))]
   ["SEG DATA" (values lexeme 'hash-colon-keyword #f (pos lexeme-start) (pos lexeme-end))]
   ["SEG TEXT" (values lexeme 'hash-colon-keyword #f (pos lexeme-start) (pos lexeme-end))]
   ["SEG CODE" (values lexeme 'hash-colon-keyword #f (pos lexeme-start) (pos lexeme-end))]
   ["equ" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["res" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["push" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["drop" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["swap" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["dup" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["rot" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["load" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["store" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["add" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["sub" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["mul" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["div" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["mod" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["and" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["or" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["xor" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["not" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["gt" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["ge" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["eq" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["le" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["lt" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["jump" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["return" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["call" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["when" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["unless" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["putc" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["puti" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["puts" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["getc" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ["halt" (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ;; TODO add hex/oct/bin representation of numbers?
   [(:+ numeric) (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   ["'\\t'" (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   ["'\\n'" (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   ["'\\f'" (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   ["'\\r'" (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   [(from/to #\' #\') (values lexeme 'constant #f (pos lexeme-start) (pos lexeme-end))]
   [(from/to #\" (:: (:~ #\\) #\"))
    (values lexeme 'string #f (pos lexeme-start) (pos lexeme-end))]
   [identifier (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end))]
   [any-char (values lexeme 'other #f (pos lexeme-start) (pos lexeme-end))]))


(define (color-ssm port offset color-mode)
  (let-values ([(lex cat paren start end) (ssm-color-lexer port)])
    (values lex cat paren start end 0 #f)))
(provide color-ssm)