#lang racket

(define identity (λ (x) x))
(provide identity)

(define-syntax (compose stx)
  (syntax-case stx ()
    [(_ f g) #'(λ (x) (f (g x)))]))
(provide compose)

(define-syntax (compose-> stx)
  (syntax-case stx ()
    [(_ f) #'f]
    [(_ f fs ...) #'(compose (compose-> fs ...)
                             f)]))
(provide compose->)