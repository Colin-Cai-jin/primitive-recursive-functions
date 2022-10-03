;The oldest version which is very slow
(library (recbase-old)
(export z s p comp primitive-rec call)
(import (scheme))

;Three basic functions

;zero function
(define (z n) 0)
;successor function
(define (s n) (+ n 1))
;projection function
(define p
 (lambda (k i)
  (lambda s
   (if (= k (length s))
    (list-ref s (- i 1))
    (begin
      (display "ARGS ERROR\n")
      (exit 0))))))


;Two ways to make new functions

;composite
(define (comp g . h)
 (lambda s
  (apply g (map (lambda (f) (apply f s)) h))))
;primitive recursive
(define (primitive-rec g h)
 (define (delete-last s)
  (let it ((r '())
	   (s s))
   (if (null? (cdr s))
    (reverse r)
    (it (cons (car s) r) (cdr s)))))
 (define (last s)
  (if (null? (cdr s))
   (car s)
   (last (cdr s))))
 (lambda s
  (let ((s2 (delete-last s))
	(n (last s)))
   (if (zero? n)
    (apply g s2)
    (apply h (append s2 (list (- n 1) (apply (primitive-rec g h) (append s2 (list (- n 1)))))))))))

;call it
(define (call f . s)
 (apply f s))
)
