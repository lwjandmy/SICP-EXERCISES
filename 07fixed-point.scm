; 寻找方程不动点

(define (fixed-point f first-guess)

  (define tolerance 0.00001)

  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))

  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))

  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
			 1.0)
