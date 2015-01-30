; mutex 互斥元

(define (make-mutex)
  (let ((cell (list false)))
	(define (the-mutex m)
	  (cond ((eq? m 'acquire)
			 (if (test-and-set! cell)
				 (the-mutex 'acquire))) ; retry
			((eq? m 'release) (clear! cell))))
	the-mutex)

  (define (clear! cell)
	(set-car! cell false))

  (define (test-and-set! cell) ; 要保证以原子操作方式调用
	(if (car cell)
		true
		(begin (set-car! cell true)
			   false))))


; serializer 串

(define (make-serializer) ; 函数make-serializer 返回一个匿名函数lambda(p) 附带了并发函数p
										; 匿名函数lambda(p)返回函数serialized-p 附带了参数args
										; 函数serialized-p 调用p以及用参数args 返回返回值val
										; 太高深，太紧密了
  (let ((mutex (make-mutex)))
	(lambda (p)
	  (define (serialized-p . args)
		(mutex 'acquire)
		(let ((val (apply p args)))
		  (mutex 'release)
		  val))
	  serialized-p)))








; 并发访问模拟

(define x 10)

(define s (make-serializer))

(paraller-execute (s (lambda () (set! x (* x x))))
				  (s (labmda () (set! x (+ x 1)))))
