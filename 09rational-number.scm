										; 有理数的算数运算

										; 英语单词
										; rational number  有理数
										; numerator  分子
										; denominator  分母

										; 有理数的构造和选择
										; (make-ret <n> <d>) 返回一个有理数，分子是整数<n>，分母是整数<d>
										; (numer <r>) 返回有理数<r>的分子
										; (denom <r>) 返回有理数<r>的分母
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

										; 有理数的运算
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))


										; 应用
(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))
