#lang scheme
#|
    Question: 1
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

(define (stream-concat s1 s2 s3)
  (if (stream-null? s1)
      the-empty-stream
      (cons-stream (list (stream-car s1)
                         (stream-car s2)
                         (stream-car s3))
                   (stream-concat (stream-cdr s1)
                                  (stream-cdr s2)
                                  (stream-cdr s3)))))


#|----------- implementation of lmn procedure to solve Forbenius coin problem|#

(define (lmn p1 p2 p3)
  
  ;procedure to check if a sum is possible from all the three coins
  (define (bill-possible? sum coin1 coin2 coin3)
    (cond ((< sum 0) #f)
          ((= sum 0) #t)
          (else (or (bill-possible? (- sum coin1) coin1 coin2 coin3)
                    (bill-possible? (- sum coin2) coin1 coin2 coin3)
                    (bill-possible? (- sum coin3) coin1 coin2 coin3)))))
  
  ;generator function for impossible-sum-stream
  (define (impossible-sum-stream-gen n coin1 coin2 coin3)
    (cond ((< n 0) the-empty-stream)
          ((bill-possible? n coin1 coin2 coin3)
           (impossible-sum-stream-gen (- n 1) coin1 coin2 coin3))
          (else
           (cons-stream n (impossible-sum-stream-gen (- n 1) coin1 coin2 coin3)))))
  
  (define impossible-sum-stream (impossible-sum-stream-gen (* p1 p2 p3) p1 p2 p3))
  
  (if (stream-null? impossible-sum-stream)
      (display "Every sum is possible --> you can buy all the ice-creams!\n")
      (begin
        (display (stream-car impossible-sum-stream))
        (newline))))

(display "Let's see the working of (lmn p1 p2 p3) procedure\n")
(display "-------- Example 1 --------\n")
(display "for (p1, p2, p3) = (5, 11, 13)\n")
(lmn 5 11 13)

(display "-------- Example 2 --------\n")
(display "for (p1, p2, p3) = (5, 7, 19)\n")
(lmn 5 7 19)

(display "-------- Example 3 --------\n")
(display "for (p1, p2, p3) = (1, 7, 8)\n")
(lmn 1 7 8)







