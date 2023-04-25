#lang scheme
(require rnrs)

#|
Name: Rustam Narayan
Roll: B20128
Question2: implementation of double-ended-queue
Assignment 3
|#

#|----------------------- Step1: Define procedure --------------------|#

(define (doubly-ended-queue)
  (define (make-deque-node data)(cons data (cons '() '())))
  (define (node-forward-ptr node)(cddr node))
  (define (set-node-forward-ptr! node ptr)(set-mcdr! (cdr node) ptr))
  (define (node-backward-ptr node)(cadr node))
  (define (set-node-backward-ptr! node ptr)(set-mcar! (cdr node) ptr))
  (define (node-data node)(car node))
  (define (node-print node)
    (display (node-data node))
    (display " "))
  
  ;implementation of constructor, predicate, selectors and mutators
  (let ((front-ptr '())
        (rear-ptr  '()))
    (define (set-front-ptr! item)(set! front-ptr item))
    (define (set-rear-ptr!  item)(set! rear-ptr  item))
    (define (empty-deque?)(null? front-ptr))
    
    (define (front-deque)
      (if (empty-deque?)
        (error "front-deque called with empty deque")
        (node-data front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
        (error "rear-deque called with empty deque")
        (node-data rear-ptr)))

    (define (front-insert-deque! item)
      (let ((new-pair (make-deque-node item)))
        (cond ((empty-deque?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-node-forward-ptr! new-pair front-ptr)
                (set-node-backward-ptr! front-ptr new-pair)
                (set-front-ptr! new-pair)))))
    
    (define (rear-insert-deque! item)
      (let ((new-pair (make-deque-node item)))
        (cond ((empty-deque?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-node-forward-ptr! rear-ptr new-pair)
                (set-node-backward-ptr! new-pair rear-ptr)
                (set-rear-ptr! new-pair)))))
    
    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error "front-delete-deque! called with an empty deque"))
            ((eq? front-ptr rear-ptr)
             (set! front-ptr '())
             (set! rear-ptr '()))
            (else
              (let ((next-node (node-forward-ptr front-ptr)))
                (set-node-backward-ptr! next-node '())
                (set-node-forward-ptr! front-ptr '())
                (set! front-ptr next-node)))))
    
    (define (rear-delete-deque!)
      (cond ((empty-deque?)
             (error "rear-delete-deque! called with an empty deque"))
            ((eq? front-ptr rear-ptr)
             (set! front-ptr '())
             (set! rear-ptr '()))
            (else
              (let ((prev-node (node-backward-ptr rear-ptr)))
                (set-node-forward-ptr! prev-node '())
                (set-node-backward-ptr! rear-ptr '())
                (set! rear-ptr prev-node)))))
    
    (define (print-deque)
      (let ((node-iter front-ptr))
        (define (print-node node)
          (cond ((not (eq? node '()))
                 (node-print node)
                 (print-node (node-forward-ptr node)))))
        (print-node node-iter)
        (newline)))
    (define (dispatch m)
      (cond ((eq? m 'empty-deque? )  empty-deque?)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)
            ((eq? m 'front-deque  )   front-deque)
            ((eq? m 'rear-deque  )   rear-deque)
            ((eq? m 'print-deque  )   print-deque)
            (else (error "Unknown message to dispatch ERROR" m))))
    dispatch))

#|---------- Step2: verify the working of procedure ---------------------------|#

(display "Example 1\n")
(define q1 (doubly-ended-queue))
((q1 'rear-insert-deque!) 1)
(display "rear-insert-deque! 1\n")
((q1 'print-deque))

((q1 'front-insert-deque!) 2)
(display "front-insert-deque! 2\n")
((q1 'print-deque))

((q1 'front-insert-deque!) 3)
(display "front-insert-deque! 3\n")
((q1 'print-deque))

((q1 'rear-insert-deque!) -1)
(display "rear-insert-deque! -1\n")
((q1 'print-deque))

((q1 'front-insert-deque!) 4)
(display "front-insert-deque! 4\n")
((q1 'print-deque))

((q1 'front-delete-deque!))
(display "front-delete-deque!\n")
((q1 'print-deque))
((q1 'rear-delete-deque!))
(display "rear-delete-deque!\n")
((q1 'print-deque))


(display "\nExample 2\n")
(define q2 (doubly-ended-queue))
((q2 'rear-insert-deque!) 'a)
(display "rear-insert-deque! a\n")
((q2 'print-deque))

((q2 'front-insert-deque!) 2)
(display "front-insert-deque! 2\n")
((q2 'print-deque))

((q2 'front-insert-deque!) 'b)
(display "front-insert-deque! b\n")
((q2 'print-deque))

((q2 'rear-insert-deque!) -1)
(display "rear-insert-deque! -1\n")
((q2 'print-deque))

((q2 'front-insert-deque!) 'c)
(display "front-insert-deque! c\n")
((q2 'print-deque))
(display "front-deque\n")
((q2 'front-deque))

(display "rear-deque\n")
((q2 'rear-deque))

((q2 'front-delete-deque!))
(display "front-delete-deque!\n")
((q2 'print-deque))
((q2 'rear-delete-deque!))
(display "rear-delete-deque!\n")
((q2 'print-deque))













