#lang scheme

#|
Name: Rustam Narayan
Roll: B20128
Question3: GCD of Numbers and polynomial expressions
Assignment 2
|#

; gcd function :  to compute GCD of two numbers

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))



; gcd-poly : to compute GCD of two polynomials


; polynomial addition
;add-poly: adds two polynomials a and b and returns the result 
(define (add-poly a b)
  (if (< (caar a) (caar b))
      (let ((new-a (re-formalize a (caar b) '())))
      (map (lambda (x y) (cons (car x) (+ (cdr x) (cdr y)))) b new-a)
      )
      (let ((new-b (re-formalize b (caar a) '())))
      (map (lambda (x y) (cons (car x) (+ (cdr x) (cdr y)))) a new-b)
      )
  )
)

; polynomial subtraction
; sub-poly: subtracts two polynomials b from a and returns the result
(define (sub-poly a b)
  (if (< (caar a) (caar b))
      (let ((new-a (re-formalize a (caar b) '())))
      (map (lambda (x y) (cons (car x) (- (cdr x) (cdr y)))) new-a b)
      )
      (let ((new-b (re-formalize b (caar a) '())))
      (map (lambda (x y) (cons (car x) (- (cdr x) (cdr y)))) a new-b)
      )
  )
)

; deduction-chhoti and deduction-badi :  helper functions
; deduction-chhoti: removes all zero coefficients from a polynomial and returns the result
(define (deduction-chhoti poly)
  (define filtered (filter (λ (x) (or (< (cdr x) 0) (> (cdr x) 0))) poly))
  (if (null? filtered)
      filtered
      (normal-poly filtered (caar filtered))
   )
)

;deduction-badi: returns the factorization of a polynomial poly by its leading coefficient.
(define (deduction-badi poly)
  (define filtered (filter (λ (x) (or (< (cdr x) 0) (> (cdr x) 0))) poly))
  (if (null? filtered)
      filtered
      (factorization filtered)
  )
)

; factorization : utilities
; factorization: returns the polynomial poly factored by its leading coefficient.
(define (factorization poly)
  (if (null? poly) poly
    (let ((factor (cdar poly)))
      (map (λ (x) (cons (car x) (cons (/ (cdr x) factor) '()))) poly)
    )
  )
)

; re-formalization :
; re-formalize: changes the degree of polynomial poly to deg and returns the result
(define (re-formalize poly deg res)
  (if (= (caar poly) deg)
      (append res poly)
      (let ((new-res (append res (cons (cons deg 0) '()))))
        (re-formalize poly (- deg 1) new-res)
      )
  )
)

; poly-init operation
; poly-init: initializes a polynomial of degree max-power with all zero coefficients and returns the result
(define (poly-init max-power res)
  (if (= max-power -1)
      res
      (let ((new-res (append res (cons (cons max-power 0) '()))))
        (poly-init (- max-power 1) new-res)
      )
  )
)

; normalization
; set-init-poly: sets the coefficient of the element in the polynomial init-poly with the same degree
; as the ele to the sum of the existing coefficient and cdr ele
(define (set-init-poly init-poly ele)
  (cond ((null? init-poly) init-poly)
        ((= (caar init-poly) (car ele)) (append (cons (cons (car ele) (+ (cdar init-poly) ( cdr ele))) '()) (cdr init-poly)))
        (else (append (cons (car init-poly) '()) (set-init-poly (cdr init-poly) ele)))
  )
)

(define (normal-poly-support poly poly-init)
  (if (null? poly)
      poly-init
      (let ((new-poly-init (set-init-poly poly-init (car poly))))
        (normal-poly-support (cdr poly) new-poly-init)
      )
  )
)

(define (normal-poly poly max-power)
  (let ((dummy-poly (poly-init max-power '())))
      (normal-poly-support poly dummy-poly)
  )
)

; polynomial multiplication
; term-wise-mul: performs the term-wise multiplication of polynomials a and b and returns the result
(define (term-wise-mul a b res)
  (if (null? b)
      res
      (if (null? res)
          (let ((new-res (cons (cons (+ (car a) (caar b)) (* (cdr a) (cdar b))) '())))
              (term-wise-mul a (cdr b) new-res)
           )
          (let ((new-res (append res (cons (cons (+ (car a) (caar b)) (* (cdr a) (cdar b))) '()))))
            (term-wise-mul a (cdr b) new-res)
          )
      )
  )
)

(define (poly-mul-support poly1 poly2 res)
  (if (null? poly1)
      res
      (let ((new-res (term-wise-mul (car poly1) poly2 res)))
          (poly-mul-support (cdr poly1) poly2 new-res)
      )
  )
)

(define (poly-mul poly1 poly2)
  (if (or (null? poly1) (null? poly2))
      '()
      (normal-poly (poly-mul-support poly1 poly2 '()) (+ (caar poly1) (caar poly2)))
   )
)


; euclidean-division
; euc-divi: performs the Euclidean division of polynomials poly1 by poly2, returns the quotient q and remainder r
(define (euc-divi poly1 poly2 q r)
  (if (null? r) (cons q r)
      (let (
            (d (caar poly2))
            (c (cdar poly2))
            )
        (begin
          (define s (cons (cons (- (caar r) d) (/ (cdar r) c)) '()))
          (define quo2 (append q s))
          (define mul-res (poly-mul s poly2))
          (define rem2 (sub-poly r mul-res))
          (if (< (caar r) d)
              (cons q r)
              (euc-divi poly1 poly2 quo2 (deduction-chhoti rem2))
              )
          )
        )
  )
)

; gcd-helper
(define (gcd-helper a b)
  (if (or (null? b) (zero? (caar b)))
      a
      (gcd-helper b (cdr (euc-divi a b '() a)))))

; Sort and normalize 
(define (normal-sort poly)
  (define sorted-sid (sort poly (λ (x y) (> (car x) (car y)))))
  (if (or (null? poly) (null? (car poly))) '()
   (normal-poly sorted-sid (caar sorted-sid))   
  )
)

; formating the output
(define (format-out poly res)
  (if (null? poly) res
      (let ((new-res (append res (cons (cons (caar poly) (cadar poly)) '()))))
        (format-out (cdr poly) new-res)
      )
  )
)

; gcd-poly: combining all the above procedure to deduce gcd of polynomials
(define (gcd-poly poly1 poly2)
  (let ((a (normal-sort (format-out poly1 '())))
        (b (normal-sort (format-out poly2 '())))
        )
    (if (or (null? a) (null? b))
       (if (null? a) b a
       )
      (if (< (caar a) (caar b))
        (deduction-badi (gcd-helper b a))
        (deduction-badi (gcd-helper a b))
      )
    )
  )
)



; Testing the correctness of GCD function on numbers
(display "outputs of gcd of numbers :\n")
(gcd 360 84)   ; expected output: 12
(gcd 17 19) ; expected output: 1
(gcd 306 75) ; expected output: 3



; Looking for output of poly-gcd on polynomials
(display "\noutputs of poly-gcd :\n")
; example1 : x^2 - 5x - 6 = (x + 1)(x - 6)
(display (gcd-poly '((2 1) (1 -5) (0 -6)) '((1 1) (0 -6)))); expected output: ((1 1) (0 -6)) i.e. (x -6) 
(newline)
; example2 : x^3 + x^2 = (x^2 +x)(x)
(display (gcd-poly '((3 1) (2 1)) '( (2 1) (1 1)))) ; expected output: ( (2 1) (1 1)) i.e. (x^2 + x)
















