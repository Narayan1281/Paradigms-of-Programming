#lang scheme
(require rnrs)

#|
Name: Rustam Narayan
Roll: B20128
Question1: implement check-infi-loop
Assignment 3
|#

#|----------------------- Step1: Define procedure --------------------|#
; Here we need to write a procedure which can detect cycle in a list
; cycle may start at any arbitrary position

(define (check-infi-loop lst)
  (define (check-loop slow fast)
    (cond ((null? fast) #f)
          ((eq? slow fast) #t) ; there is a loop iff the slow pointer is equal to fast
          ((null? (cdr fast)) #f)
          ((null? (cdr (cdr fast))) #f)
          (else (check-loop (cdr slow) (cdr (cdr fast))))))
  (check-loop lst (cdr lst)))

; procedure to access last element/pointer of the list
(define (last-pair x) 
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; End to end cycle i.e last->fast
(define (make-cycle1 x)
  (set-mcdr! (last-pair x) x)
  x)

; cycle in between 
(define (make-cycle2 x)
  (set-mcdr! (last-pair x) (cdr x)) ; x should we of atleat size=2
  x)

; procedure to print the list
(define (print-list x)
  (cond ((null? x) (display "\n"))
        (else
         (begin (display (car x))
                (display " ")
                (print-list (cdr x))))))

#|---------- Step2: verify the working of procedure ---------------------------|#

(define nums (list 1 2 5 8 79 12)) ; linear list
(define cycle-nums1 (make-cycle1 (list -1 0 2 3 71 100))) ; end-to-end cycle
(define cycle-nums-single (make-cycle1 (list 1))) ; end-to-end cycle with single element
(define cycle-between-nums (make-cycle2 (list 1 0 -4 -7 12))) ; cycle in between the nums

(display "Let's check if check-infi-loop works!\n")
(display "Example1: nums\n")
(check-infi-loop nums) ;Expected output: #f
(print-list nums) ; displays the array for verification
(display "Example2: cycle-nums1\n")
(check-infi-loop cycle-nums1) ;Expected output: #t
(display "Example3: cycle-nums-single\n")
(check-infi-loop cycle-nums-single) ;Expected output: #t
(display "Example4: cycle-between-nums\n")
(check-infi-loop cycle-between-nums) ;Expected output: #t






