#lang scheme

#|
Name: Rustam Narayan
Roll: B20128
Question1: Implementing arithmetic operations on church numerals
Assignment 2
|#


; Defining Basic Church numerals

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;  church-add:- Add two church numbers

(define (church-add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))


(define (rec-helper arg1 arg2)
  (lambda (f)
    (lambda (x)
       ((arg1 (arg2 f)) x))))

; church-mul:- Multiply two or more church numbers
(define (church-mul . nums)
  (define (mul-helper acc lst)
    (if (null? lst)
        acc
        (mul-helper (rec-helper acc (car lst)) (cdr lst))))
  (mul-helper (car nums) (cdr nums)))

; display-church:- Display the church number as an decimal
(define (display-church n)
  ((n (lambda (x) (+ x 1))) 0))

(define two (add-1 (add-1 zero)))
(define three (add-1 two))
(define four (add-1 three))
(define five (add-1 four))
(define six (add-1 five))
(define seven (add-1 six))
(define eight (add-1 seven))
(define nine (add-1 eight))


(display "Church numerals into action :\n")
(display-church (church-add three three))  ; expected output: 6
(display-church (church-mul two four three five)) ;expected output: 120
(display-church (church-mul nine four)) ; expected output: 36
(display-church (church-mul six four)) ; expected output: 24






