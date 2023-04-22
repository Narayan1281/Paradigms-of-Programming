#lang scheme
(define (forloop start end inc)
  (if (< start end)
      ;here begin key word is required
      ;application: not a procedure
      ;expected a procedure that can be applied to arguments
      (begin (display start)
        (newline)
        (forloop (+ start inc) end inc))
      (display "")
      ))

(forloop 2 21 2)