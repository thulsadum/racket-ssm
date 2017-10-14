#lang racket

(require racket/contract)
(require "private/state.rkt"
         "private/instructions.rkt"
         "private/functional.rkt")

(provide #%app #%datum)
(provide #%top-interaction)
(provide (all-from-out "private/instructions.rkt"))


;; TODO syntax coloring! :)


(module+ test
  (require rackunit))


(define-syntax (compose-segments stx)
  (syntax-case stx ()
    [(_ seg) #'seg]
    ; [(_ seg-f seg-g) #'(compose seg-g seg-f)]
    [(_ seg-f segs ...) #'(compose (compose-segments segs ...) seg-f)]))
(provide compose-segments)

;; program execution
(define-syntax-rule (pc+1 st)
  (update-hstate st #:pc add1))

(define (step st)
  (let* ([pc (hstate-pc st)]
         [instr (vector-ref (hstate-program st) pc)]
         [st (pc+1 st)])
    (instr st)))
(provide step)

(define (run st)
  (if (eq? (hstate-exception st) 'none)
      (run (step st))
      (hstate-exception st)))
(provide run)    


;; module-begin
(define-syntax-rule (ssm-mb (program segment ...))
  (#%module-begin
   ; Compile Time state
   (define cstate ((compose-segments segment ...) empty-state))
   #;(println cstate)
   ; Runtime-state
   (define rstate (state->hstate cstate))
   #;(println rstate)
   (define (do-step)
     (set! rstate (step rstate)))
   (define (do-run)
     (run rstate))
   (module* main #f (do-run))
   (provide (rename-out (do-run run) (do-step step)))))
(provide (rename-out [ssm-mb #%module-begin]))