#lang scheme
(define (isort nums)
  (define (sorted-insert num arr)
    (cond ((null? arr) (list num))
          ((> (car arr) num) (cons num arr))
          (else (cons (car arr) (sorted-insert num (cdr arr))))
          )
    )
  (define (insertion-sort nums arr)
    (cond ((null? nums) arr)
          (else (insertion-sort (cdr nums) (sorted-insert (car nums) arr)))
          )
    )
  ;calling insertion-sort procedure
  ; arr = '() --> empty sorted list
  (insertion-sort nums '())
    )

(isort '(12 8 0 12 2 3 19 18))