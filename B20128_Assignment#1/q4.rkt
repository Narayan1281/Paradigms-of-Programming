#lang scheme
; isort : insertion sort procedure to sort input list
; with custom comparator
(define (isort nums comparator)
  (define (sorted-insert num arr)
    (cond ((null? arr) (list num))
          ((not (comparator (car arr) num)) (cons num arr))
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

; qsort : a procedure to sort a list of numbers using quicksort
; with custom comparator
(define (qsort nums comparator)
  (cond
    ((or (null? nums)
        (null? (cdr nums)))
    nums) ; empty array or array with single-element is always sorted
  (else
   (let ((pivot (car nums)))
   (append (qsort (filter (lambda (x) (comparator x pivot)) (cdr nums)) comparator)
           ; select smaller element and sort recursively
            (list pivot)
            (qsort (filter (lambda (x) (not (comparator x pivot))) (cdr nums)) comparator)
           ; select larger element and sort recursively
    )))
  ))

; custom string comparator
(define (strings-comparator s1 s2)
  (string<? s1 s2)) 

(define (compare-make-rectangular x y)
  (let ((x-abs (magnitude x))
        (y-abs (magnitude y))
        (x-real (real-part x))
        (y-real (real-part y)))
    (cond ((> x-abs y-abs) #t)
          ((< x-abs y-abs) #f)
          ((> x-real y-real) #t)
          ((< x-real y-real) #f)
          (else #f))))

(define (gensort input comparator sorting-algorithm)
    (sorting-algorithm input comparator))

(define num-array '(23 -4 12 78 0 -123 23 0))
(define string-array '("rustam" "v" "iit" "nit" "508" "19"))
(define complex-array (list (make-rectangular 6 8) (make-rectangular 3 4) (make-rectangular 8 6) (make-rectangular 12 5) (make-rectangular 5 12)))

(gensort num-array < qsort)
(gensort string-array strings-comparator qsort)
(gensort complex-array compare-make-rectangular qsort)

(gensort num-array > isort)
(gensort string-array strings-comparator isort)
(gensort complex-array compare-make-rectangular isort)

