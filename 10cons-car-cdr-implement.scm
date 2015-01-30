(define (cons x y)
  (define (dispatch m)
	(cond ((= m 0) x)
		  ((= m 1) y)
		  (else (error "Argument not 0 or 1 -- cons" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))




(define pair (cons 1 2))
(display (car pair))
(display (cdr pair))
(car pair)
(cdr pair)
