#lang scheme
; qsort : a procedure to sort a list of numbers using quicksort
(define (qsort nums)
  (cond
    ((or (null? nums)
        (null? (cdr nums)))
    nums) ; empty array or array with single-element is always sorted
  (else
   (let ((pivot (car nums)))
   (append (qsort (filter (lambda (x) (< x pivot)) (cdr nums)))
           ; select smaller element and sort recursively
            (list pivot)
            (qsort (filter (lambda (x) (>= x pivot)) (cdr nums)))
           ; select larger element and sort recursively
    )))
  ))

(qsort '(23 -4 12 78 0 -123 23 0))