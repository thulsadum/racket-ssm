#lang racket

(module+ test
  (require rackunit))

; internal state willbe transformed to hstate (harden-state) after initialization
(struct state (symbol-table strings static-memory program
                            no-text no-data no-code) #:transparent)
(provide (struct-out state))

(struct hstate (symbol-table strings static-memory program pc stack exception) #:transparent)
(provide (struct-out hstate))

(define empty-state (state (hash)   ; symtab
                           (list)   ; strtab
                           (list)   ; smem
                           (list)   ; program
                           0        ; no-text
                           0        ; no-data
                           0))      ; no-code
(provide empty-state)


(define (update-state* s
                       #:symbol-table (st state-symbol-table)
                       #:strings (ss state-strings)
                       #:static-memory (smem state-static-memory)
                       #:program (prog state-program)
                       #:no-text (ntext state-no-text)
                       #:no-data (ndata state-no-data)
                       #:no-code (ncode state-no-code))
  (state (st s)
         (ss s)
         (smem s)
         (prog s)
         (ntext s)
         (ndata s)
         (ncode s)))

(define (update-state s
                      #:symbol-table (st identity)
                      #:strings (ss identity)
                      #:static-memory (smem identity)
                      #:program (prog identity)
                      #:no-text (ntext identity)
                      #:no-data (ndata identity)
                      #:no-code (ncode identity))
  (update-state* s
                 #:symbol-table (compose st state-symbol-table)
                 #:strings (compose ss state-strings)
                 #:static-memory (compose smem state-static-memory)
                 #:program (compose prog state-program)
                 #:no-text (compose ntext state-no-text)
                 #:no-data (compose ndata state-no-data)
                 #:no-code (compose ncode state-no-code)))
(provide update-state update-state*)

(module+ test
  (check-equal? (update-state empty-state) empty-state)
  (check-equal? (update-state* empty-state) empty-state))


(define-syntax-rule (code+1 st)
  (update-state st #:no-code add1))
(define-syntax-rule (text+1 st)
  (update-state st #:no-text add1))
(define-syntax-rule (data+1 st)
  (update-state st #:no-data add1))
(provide code+1 text+1 data+1)

(module+ test
  (check-equal? (state-no-code (code+1 empty-state)) 1)
  (check-equal? (state-no-text (text+1 empty-state)) 1)
  (check-equal? (state-no-data (data+1 empty-state)) 1))

(define (update-hstate* s
                        #:symbol-table (st hstate-symbol-table)
                        #:strings (ss hstate-strings)
                        #:static-memory (smem hstate-static-memory)
                        #:program (prog hstate-program)
                        #:pc (pc hstate-pc)
                        #:stack (stack hstate-stack)
                        #:exception (ex hstate-exception))
  (hstate (st s)
          (ss s)
          (smem s)
          (prog s)
          (pc s)
          (stack s)
          (ex s)))

(define (update-hstate s
                       #:symbol-table (st identity)
                       #:strings (ss identity)
                       #:static-memory (smem identity)
                       #:program (prog identity)
                       #:pc (pc identity)
                       #:stack (stack identity)
                       #:exception (ex identity))
  (update-hstate* s
                  #:symbol-table (compose st hstate-symbol-table)
                  #:strings (compose ss hstate-strings)
                  #:static-memory (compose smem hstate-static-memory)
                  #:program (compose prog hstate-program)
                  #:pc (compose pc hstate-pc)
                  #:stack (compose stack hstate-stack)
                  #:exception (compose ex hstate-exception)))
(provide update-hstate update-hstate*)

(module+ test
  (check-equal? (update-hstate (state->hstate empty-state)) (state->hstate empty-state))
  (check-equal? (update-hstate* (state->hstate empty-state)) (state->hstate empty-state)))

(define (state->hstate s)
  (define list->ivec (λ (xs) (vector->immutable-vector (list->vector xs))))
  (define s-prime (update-state s
                                #:strings list->ivec
                                #:static-memory list->vector
                                #:program list->ivec))
  (hstate (state-symbol-table s-prime)
          (state-strings s-prime)
          (state-static-memory s-prime)
          (state-program s-prime)
          0           ; pc
          empty       ; stack
          'none))     ; exception
(provide (contract-out (state->hstate (state? . -> . hstate?))))

(module+ test
  (check-equal? (state->hstate empty-state)
                (hstate (hash)
                        #()
                        #()
                        #()
                        0
                        '()
                        'none)))

