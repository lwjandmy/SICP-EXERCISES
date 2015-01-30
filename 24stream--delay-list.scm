; 流是带有延迟求值的序列结构
; 对函数加上记忆功能 memo-proc

; 流使我们利用各种序列操作，但又不会带来将序列作为表去操作而引起的代价
; 流 = 序列操作的优雅，统一 + 执行效率递增，而不是一直保持很低
; 流的基本想法：
;  部分构造出流结构，当使用者需要访问未构造出的那部分时，流自动向下构造
;  但向下构造时只构造出满足当时需要的一小部分



; memo-proc 对过程进行记忆功能的包装
(define (memo-proc proc)
  (let ((already-run? false) (result false)) ; already-run标记是否执行过，result记录执行结果
	(lambda() ; 此函数作为返回值，这个函数附带了already-run和result两个变量
	  (if (not already-run?)
		  (begin (set! result (proc))
				 (set! already-run? true)
				 result)
		  result))))


(define (delay proc) ; 对过程proc进行延时，即返回一个延时对象，对延时对象的调用就是对过程proc的调用
  proc) ;; 把要调用的过程直接返回

(define (force delayed-object) ; 延时对象作为参数，需要求值时直接调用这个延时函数
  (delayed-object))



; 流
(define (cons-stream s1 s2) (cons s1 (delay s2))) ; 注意s2必须是一个函数，以这个函数的返回值为cdr
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


; 流 测试
(define test-proc (memo-proc (lambda() (display "execute") 2))) ; 我想返回2，但这里需要一个函数，所以用lambda生成一个
(define test-stream (cons-stream 1 test-proc)) ; 注意cdr必须是一个函数
(stream-car test-stream)
(stream-cdr test-stream)
(stream-car test-stream)
(stream-cdr test-stream)


