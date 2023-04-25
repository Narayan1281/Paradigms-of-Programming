#lang scheme

#|
    Question: 3
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

(define (square x) (* x x))

; generator function for alternate-factorial stream
(define (alt-fact-gen a b)
    (cons-stream a (alt-fact-gen (* a (+ b 1) (+ b 2)) (+ b 2))))

; odd terms of factorial stream
(define odd-facts (alt-fact-gen 1 1))

; even terms of factorial stream
(define even-facts (cons-stream 1 (alt-fact-gen 2 2)))

; generator function for alternate-power of any x
(define (alt-pow-x-gen x sq-x)
    (cons-stream x (alt-pow-x-gen (* x sq-x -1) sq-x)))

(define (pow-x-sin x)
    (alt-pow-x-gen x (square x)))

(define (pow-x-cos x)
    (cons-stream 1 (stream-map - (alt-pow-x-gen (square x) (square x)))))

;ith term of (sin x) stream is the sum of first i terms of the mathematical function (sin x)
; x - (x^3)/3! + (x^5)/5! - ...
(define (sin x)
    (define (sin-val x)
      (2-stream-map / (pow-x-sin (* x 1.0)) odd-facts))
    (define sinx-st
      (2-stream-map (lambda (x y) (+ x y))
                  (sin-val x)
                  (cons-stream 0 sinx-st)))
    sinx-st)

;ith term of (cos x) stream is the sum of first i terms of the mathematical function (cos x)
; 1 - (x^2)/2! + (x^4)/4! - ...
(define (cos x)
    (define (cos-val x)
      (2-stream-map / (pow-x-cos (* x 1.0)) even-facts))
    (define cosx-st
      (2-stream-map (lambda (x y) (+ x y))
                  (cos-val x)
                  (cons-stream 0 cosx-st)))
    cosx-st)

; iden x
; sum of squares of each term of (sin x) and (cos x)
; (sin x)^2 + (cos x)^2 = 1
; value converges to 1 after sum iteration
 (define (iden x)
    (2-stream-map + (stream-map square (sin x))
                  (stream-map square (cos x))))


(display "Let's see the working of (sin x) function\n")
(display "-------- Example 1 --------\n")
(display "(sin x) for x = 1.57 ~ (90 degree)\n")
(display-first-k 5 (sin 1.57))
(newline)

(display "-------- Example 2 --------\n")
(display "(cos x) for x = 0.785 ~ (45 degree)\n")
(display-first-k 5 (cos 0.785))

(display "\n-------- Example 3 --------\n")
(display "(iden x) for x = 3.14 ~ (180 degree)\n")
(display-first-k 5 (iden 1))

(display "\n-------- Example 4 --------\n")
(display "accessing 100th element of (cos 2*pi)\n")
(stream-ref 99 (cos 6.28))



#|
   Note: In this code stream is implemented from scratch. This code does not uses any library
         to implement procedures involving streams.
        --> Initial implementation of stream is taken from Manas Thakur's previous offering of course.
|#





