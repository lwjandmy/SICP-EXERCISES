
(define x '((a b) c d))

(define y '(e f))

x
(set-car! x y)
x


(define x (list 'a 'b))
(define z1 (cons x x)) ; z1 的car和cdr都指向p(x)
x
z1
(define z2 (cons (list 'a 'b) (list 'a 'b))) ; z2的car和cdr都指向p('(a b))，但p('(a b))是不同的，虽然说两个不同的p('(a b))，它们指向的内容都是'(a b)
z2


(define (set-to-xxx! x)
  (set-car! (car x) 'xxx)
  x)
(set-to-xxx! z1) ; 修改了p(x)，而z1的car和cdr指向同一个p(x)，所以结果是两个xxx
(set-to-xxx! z2) ; 修改了car指向的p(x)，所以结果只有一个xxx
