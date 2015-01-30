
; 线路对象，负责事件驱动

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action=procedure! proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc))
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	dispatch))

(define (call-each procedures)
  (if (null? procedures)
	  'done
	  (begin
		((car procedures))
		(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


; 下面的与或非模拟电路都乎略了延时，延时代码太复杂，太长
; 非
(define (inverter input output)
  (define (invert-input)
	(let ((new-value (logital-not (get-signal input))))
	  (set-signal! output new-value)))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
		((= s 1) 0)
		(else (error "Invalid signal" s))))

; 与
(define (add-gate a1 a2 output)
  (define (add-action-procedure)
	(let ((new-value
		   (logical-and (get-signal a1) (get-signal a2))))
	  (set-signal! output new-value)))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond ((and (= a1 1) (= a2 1)) 1)
		(else 0)))

; 或
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value
		   (logical-or (get-signal a1) (get-signal a2))))
	  (set-signal! output new-value)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond ((and (= a1 0) (= a2 0)) 0)
		(else 1)))


; 半加器
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
	(or-gate a b d)
	(and-gate a b c)
	(inverter c e)
	(and-gate d e s)
	'ok))

; 全加器
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
		(c1 (make-wire))
		(c2 (make-wire)))
	(half-adder b c-in s c1)
	(half-adder a s sum c2)
	(or-gate c1 c2 c-out)
	'ok))
