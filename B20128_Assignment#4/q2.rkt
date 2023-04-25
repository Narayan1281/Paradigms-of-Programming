#lang scheme

#|
    Question: 2
    Assignment: 4
    Submitted by: Rustam Narayan (B20128)
|#

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (lambda () expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())

(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

#|------ procedure to print first k elements of a stream-------|#
(define ( display-first-k k s)
  ;@params
  ;k : number of elements to be printed
  ;s : stream of elements
    (if (> k 0)
        (begin
          (display (stream-car s))
          (display " ")
          (display-first-k (- k 1) (stream-cdr s)))
        (display "\n")))

;|--------------- procedure to access (n+1)th element of a stream -------------|;
(define (stream-ref n s)
    (if (= n 0)
        (stream-car s)
        (stream-ref (- n 1) (stream-cdr s))))

; 2-stream-map
; applies a `proc` operation on corresponding elements of two streams
(define (2-stream-map proc s1 s2)
  ;@params
  ;s1 : stream of elements
  ;s2 : stream of elements
  ;proc : operation to be applied i.e. +, square, etc.
    (if (stream-null? s1)
        the-empty-stream
        (cons-stream (proc (stream-car s1) (stream-car s2))
                     (2-stream-map proc (stream-cdr s1) (stream-cdr s2)))))

;generator function for factorial stream
(define (fact-gen a b)
    (cons-stream a (fact-gen (* a b) (+ b 1))))

;generator function for integers stream
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

; stream of integers starting from 1
(define integers (integers-starting-from 1))

; stream of factorials starting from 1
(define facts (fact-gen 1 1))

;procedure : fact-rem
;returns : a stream whose
;          kth element is k! mod (k + 1)
(define (fact-rem)
    (2-stream-map remainder facts integers))

(display "Let's see the working of fact-rem procedure\n")
(display "-------- Example 1 (fact-rem stream starting from k=0) ----------\n")
; display first 10 elements of stream fact-rem
(display-first-k 10 (fact-rem))

(display "\n-------- Example 2 (fact-rem stream starting from k=1) ----------\n")
; display first 37 elements of stream fact-rem
(display-first-k 37 (stream-cdr (fact-rem)))

(display "\n-------- Example 3 (kth element of fact-rem) ----------\n")
(display "k = 10000\n")
(stream-ref 10000 (stream-cdr (fact-rem))) ; expected output: 0

(display "k = 970\n")
(stream-ref 969 (stream-cdr (fact-rem))) ; expected output: 970 as 971 is a prime


#| Obeservation: 1
      output of fact-rem is non-zero for all k s.t. (k+1) is a prime number >=5
|#

#|
   Note: In this code stream is implemented from scratch. This code does not uses any library
         to implement procedures involving streams.
        --> Initial implementation of stream is taken from Manas Thakur's previous offering of course.
|#











