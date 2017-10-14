#lang racket

(require "state.rkt"
         "functional.rkt")


(define-syntax-rule (simple-instruction body)
  (λ (st) (update-state st
                        #:program (λ (prog) (append prog (list body)))
                        #:no-code add1)))
(provide simple-instruction)


(define-syntax (complex-instruction stx)
  (syntax-case stx ()
    [(_ cbody rbody)
     #'(λ (st)
         (let ([st (cbody st)])
           ((simple-instruction rbody) st)))]))
(provide complex-instruction)


;(define-syntax (compose-instructions stx)
;  (syntax-case stx ()
;    [(_ instr) #'instr]
;    [(_ instr instrs ...) #'(compose (compose-instructions instrs ...)
;                                     instr)]))
;(provide compose-instructions)

(define-syntax-rule (add-symbol label offset)
  (λ (st)
    (hash-set (state-symbol-table st)
              label
              offset)))

(define-syntax-rule (resolve-label label)
  (λ (st)
    (hash-ref (hstate-symbol-table st)
              label)))
(provide resolve-label)

(define-syntax-rule (get-string st n)
  (vector-ref (hstate-strings st) n))

;; stack manipulations
(define-syntax-rule (update-stack f)
  (λ (st)
    (update-hstate st
                   #:stack
                   f)))
(provide update-stack)

(define (push x)
  (update-stack (λ (stack) (cons x stack))))
(provide push)


(define (drop)
  (update-stack cdr))
(provide drop)

(define (top)
  (λ (st)
    (car (hstate-stack st))))
(provide top)

(define (pop)
  (λ (st)
    (values ((top) st)
            ((drop) st))))

;; memory access
(define (store-var var value)
  (λ (st)
    (update-hstate st #:static-memory (λ (mem)
                                        (vector-set! mem var value)
                                        mem))))

(define (load-var var)
  (λ (st)
    (vector-ref (hstate-static-memory st) var)))


;; segments
(define-syntax-rule (code-segment instr ...)
  (λ (st)
    ((compose-> instr ...) st)))
(provide code-segment)


(define-syntax-rule (text-segment text ...)
  (λ (st)
    ((compose-> text ...) st)))
(provide text-segment)


(define-syntax-rule (data-segment decl ...)
  (λ (st)
    ((compose-> decl ...) st)))
(provide data-segment)


;; memory allocation / init
(define-syntax (initialization stx)
  (syntax-case stx (EQU)
    [(_ EQU num ...) #'(λ (st)
                         (update-state st #:static-memory (λ (mem) (append mem (list num ...)))))]
    [(_ label EQU num ...) #'(compose (initialization EQU num ...)
                                      (λ (st)
                                        (update-state* st
                                                       #:symbol-table
                                                       (add-symbol label
                                                                   (length (state-static-memory st))))))]))
(provide initialization)

;; string definiton
(define-syntax (string-def stx)
  (syntax-case stx ()
    [(_ str) #'(λ (st) (update-state st
                                     #:strings (λ (strs) (append strs (list str)))))]
    [(_ label str) #'(compose (string-def str)
                              (λ (st) (update-state* st
                                                     #:symbol-table
                                                     (add-symbol label
                                                                 (length (state-strings st))))))]))
(provide string-def)



;; instructions
(define-syntax-rule (push-number NUM)
  (simple-instruction (push NUM)))
(provide push-number)

(define-syntax-rule (push-label LABEL)
  (simple-instruction
   (λ (st)
     (let ([num ((resolve-label LABEL) st)])
       ((push num) st)))))
(provide push-label)


; FIXME perhaps syntax/parse might help to make good exceptions?
(define-syntax-rule (dup)
  (simple-instruction (update-stack (λ (stack) (let ([top (car stack)])
                                                 (cons top stack))))))
(provide dup)


(define-syntax-rule (unless label)
  (simple-instruction
   (λ (st)
     (let-values ([(t new-st) ((pop) st)])
       (if (= 0 t)
           ((do-jump label) new-st)
           new-st)))))
(provide unless)


(define-syntax-rule (putc)
  (simple-instruction
   (λ (st)
     (let-values ([(t new-st) ((pop) st)])
       (write-char (integer->char t))
       new-st))))
(provide putc)


(define-syntax-rule (puts)
  (simple-instruction
   (λ (st)
     (let-values ([(t new-st) ((pop) st)])
       (write-string (get-string st t))
       new-st))))
(provide puts)

(define-syntax-rule (puti)
  (simple-instruction
   (λ (st)
     (let-values ([(t st) ((pop) st)])
       (write t)
       st))))
(provide puti)


(define-syntax-rule (do-jump label)
  (λ (st)
    (update-hstate* st #:pc (resolve-label label))))

(define-syntax-rule (jump label)
  (simple-instruction
   (do-jump label)))
(provide jump)

(define-syntax-rule (call label)
  (simple-instruction
   (λ (st)
     (let* ([pc (hstate-pc st)]
            [st ((push pc) st)])
       ((do-jump label) st)))))
(provide call)

(define-syntax-rule (return)
  (simple-instruction
   (λ (st)
     (let*-values ([(dest st) ((pop) st)])
       (update-hstate* st #:pc (λ (oldpc) dest))))))
(provide return)

(define-syntax-rule (halt)
  (simple-instruction
   (λ (st)
     (update-hstate st #:exception (λ (x) 'halt)))))
(provide halt)

(define-syntax-rule (load)
  (simple-instruction
   (λ (st)
     (let*-values ([(var st) ((pop) st)]
                   [(val) ((load-var var) st)])
       ((push val) st)))))
(provide load)


(define-syntax-rule (store)
  (simple-instruction
   (λ (st)
     (let*-values ([(var st) ((pop) st)]
                   [(val st) ((pop) st)])
       ((store-var var val) st)))))
(provide store)

(define-syntax-rule (add)
  (simple-instruction
   (λ (st)
     (let*-values ([(op1 st) ((pop) st)]
                   [(op2 st) ((pop) st)])
       ((push (+ op1 op2)) st)))))
(provide add)

(define-syntax-rule (instruction-with-label label instr)
  (complex-instruction
   ;; special compole time semantics
   (λ (cst)
     (let* ([cst (update-state* cst #:symbol-table (add-symbol label (state-no-code cst)))])
       (instr cst)))
   identity))
(provide instruction-with-label)
