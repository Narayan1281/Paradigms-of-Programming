#lang scheme

#|
Name: Rustam Narayan
Roll: B20128
Question2: Evaluation of Infix expressions
Assignment 2
|#

(define (eval-infix expr)
  (cond
    ((number? expr) expr)
    ((pair? expr)
     (let ((op (cadr expr)) ; second element
           (arg1 (eval-infix (car expr))) ; first element
           (arg2 (eval-infix (caddr expr)))) ; third element
       (cond
         ((eq? op '+) (+ arg1 arg2))
         ((eq? op '-) (- arg1 arg2))
         ((eq? op '*) (* arg1 arg2))
         ((eq? op '/) (/ arg1 arg2))
         (else (error "Operator not accomadated" op)))))
    (else (error "Invalid expression" expr))))


(display "Let's witness the power of eval-infix :\n")
(eval-infix '((7 * (24 - (5 * 4))) / 4)) ; expected output: 7
(eval-infix '((75 / (24 + (5 - 4))) * 4)) ; expected output: 12
(eval-infix '((12 + 8) - 7)) ; expected output: 13
(eval-infix '((7 * (13 + (5 * 4) )) / 11 )) ; expected output: 21
