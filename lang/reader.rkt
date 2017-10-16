#lang racket

(require "lexer.rkt")
(require "parser.rkt")

(define (read-syntax path in)
  (define tokenizer (make-tokenizer in))
  (define ast (parse path tokenizer))
  (define module-datum `(module ssm-mod ssm/expander ,ast))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer) (dynamic-require 'ssm/lang/colorer 'color-ssm)]
        ; [(drracket:indentation) (dynamic-require "indenter.rkt" 'indent-ssm)]
        ; [(drracket:toolbar-buttons) ···]
        [else default]))
    handle-query)
(provide get-info)