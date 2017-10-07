#lang racket

(define (read-syntax path in)
  (port->lines in)
  #'(module ssm-mod racket
      'hello))
(provide read-syntax)