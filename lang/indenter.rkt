#lang racket

(require br/indent)

(define (indent-ssm tbox [posn 0])
  (define prev-line (previous-line tbox posn))
  ; (define current-line (line tbox posn))
  (define prev-indent (or (line-indent tbox prev-line) 0))
  prev-indent)
(provide indent-ssm)