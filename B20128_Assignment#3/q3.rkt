#lang scheme
(require rnrs)

#|
Name: Rustam Narayan
Roll: B20128
Question3: serialized-exchange-nu
Assignment 3
|#

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 'pass)) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-mcar! cell true)
             false)))
#|--------------------Modified portion ---------------------|#
; added account-id
; added serialized-echange-nu

(define (make-account balance account-id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'get-account-id) account-id)
            (else (error "Unknown request -- MAKE-ACCOUNT "
                         m))))
    dispatch))

; Balance Transfer procedure
(define (exchange account1 account2) 
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; Account with lower account-id has been given priority for exchane
; and avoid deadlock

(define (serialized-echange-nu account1 account2) 
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (acc-id1 (account1 'get-account-id))
        (acc-id2 (account2 'get-account-id)))
    (if (< acc-id1 acc-id2)
        ((serializer2 (serializer1 exchange)) account1 account2)
        ((serializer1 (serializer2 exchange)) account1 account2))))




