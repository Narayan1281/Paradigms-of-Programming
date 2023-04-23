#lang scheme
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

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (map f l)
  (if (null? l)
      null
      (cons (f (car l))
            (map f (cdr l)))))

(define (filter pred l)
  (cond ((null? l) null)
        ((pred (car l)) (cons (car l) (filter pred (cdr l))))
        (else (filter pred (cdr l)))))

(define (accumulate op v l)
  (if (null? l)
      v
      (op (car l)
          (accumulate op v (cdr l)))))

(define (prime? n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (= n (smallest-divisor n)))

(define (square x) (* x x))

#| ------------ example 0 ------------|#
#|
> (car (cdr
        (filter
         prime?
         (enumerate-interval 10000 1000000))))
10009
> (stream-car (stream-cdr
               (stream-filter
                prime?
                (stream-enumerate-interval 10000 1000000))))
10009
|#

#| ----------- example 1 -------------|#

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-ref n s)
    (if (= n 0)
        (stream-car s)
        (stream-ref (- n 1) (stream-cdr s))))

#|
> integers-starting-from
#<procedure:integers-starting-from>
> integers
(1 . #<procedure:...23/POP/assn4/q2.rkt:8:31>)
> (stream-ref 10 integers)
11
> (stream-ref 517 integers)
518
> (stream-ref 100000 integers)
100001
|#


#| ----------- example 2 -------------|#
(define (divisible? x y) (= (remainder x y) 0))

(define nd4
    (stream-filter
     (lambda (x) (not (divisible? x 4)))
     integers))

#|
> nd4
(1 . #<procedure:...23/POP/assn4/q2.rkt:8:31>)
> (stream-ref 4 nd4)
6
> (stream-ref 517 nd4)
690
|#

#| ----------- example 3 -------------|#

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

#|
> (stream-ref 10 fibs)
55
> (stream-ref 100 fibs)
354224848179261915075
> (stream-ref 500 fibs)
139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
> 
|#

#| ----------- example 4 -------------|#

(define (sieve s)
    (cons-stream
    (stream-car s)
    (sieve (stream-filter
            (lambda (x) (not (divisible? x (stream-car s))))
            (stream-cdr s)))))
    
(define primes (sieve (integers-starting-from 2)))

#|
> primes
(2 . #<procedure:...23/POP/assn4/q2.rkt:8:31>)
> (stream-ref 10 primes)
31
> (stream-ref 517 primes)
3709
|#

#| ----------- example 5 -------------|#

(define primesV2
    (cons-stream 2
                 (stream-filter primeV2? (integers-starting-from 3))))

(define (primeV2? n)
    (define (iter ps)
      (cond ((> (square (stream-car ps)) n) true)
            ((divisible? n (stream-car ps)) false)
            (else (iter (stream-cdr ps)))))
    (iter primesV2))

#|
> (stream-ref 517 primesV2)
3709
> 
|#

#| ----------- example 6 -------------|#

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt x tolerance)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) tolerance))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (sqrt-improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt-stream-gen x)
    (define guesses
      (cons-stream 1.0
                   (stream-map (lambda (guess)
                                 (sqrt-improve guess x))
                               guesses)))
    guesses)
#|
> (define sqrt-stream-2 (sqrt-stream-gen 2))
> (stream-ref 0 sqrt-stream-2)
1.0
> (stream-ref 5 sqrt-stream-2)
1.414213562373095
> 
|#









