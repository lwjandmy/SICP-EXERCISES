; 牛顿法求方程不动点


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


(define (fixed-point-newtons-method g guess)

  (define dx 0.00001)

  (define (deriv g)
	(lambda (x)
	  (/ (- (g (+ x dx)) (g x))
		 dx)))

  (define (newton-transform g)
	(lambda (x)
	  (- x (/ (g x) ((deriv g) x)))))

  (fixed-point (newton-transform g) guess))

(fixed-point-newtons-method cos 1.0)
(fixed-point cos 1.0)
