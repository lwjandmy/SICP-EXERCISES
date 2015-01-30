
; map 对表中所有元素做map（映射，函数调用），返回映射结果所成的表

(define (map proc items)
  (if (null? items)
	  '() ;nil
	  (cons (proc (car items))
			(map proc (cdr items)))))

; map之所以能对表的迭代过程进行屏蔽，就是因为lisp使用了统一的数据结构（car，cdr，cons）


; map 的应用
(define (map-list proc list)
  (map proc list))
(define (map-tree proc tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (proc tree))
		(else (cons (map-tree proc (car tree))
					(map-tree proc (cdr tree))))))
(map-list
 (lambda(item) (display item))
 '(1 2 3))
(map-tree
 (lambda(item)
   (display item)
   (display " ")
   item)
 '((01 02 03) (11 12) (21)))




; enumerate 生成数

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low
			(enumerate-interval (+ low 1) high))))


; filter 过滤器

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))


; accumulate 累加器

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))





; 应用

(define (even-fibs n) ;; 偶数的Fibonacci数列
  (accumulate cons '() (filter even? (map fib
										  (enumerate-interval 0 n)))))
