#lang racket

(require "lexer.rkt")
(require "parser.rkt")

(define (read-syntax path in)
  (define tokenizer (make-tokenizer in))
  (define ast (parse path tokenizer))
  (define module-datum `(module ssm-mod ssm/expander ,ast))
  (datum->syntax #f module-datum))
(provide read-syntax)