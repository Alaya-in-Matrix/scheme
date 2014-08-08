(load "put-get.scm")
;;common
(define nil '())
(define (square x) (* x x))
;;++++++++++++++++++++++++++++++++++++++exercise 2.73++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (put <op> <type> <item>)
;; (get <op> <type>)
(define (deriv exp var)
  (cond ((number? exp) 0)
		(variable? exp) (if (same-variable? exp var) 1 0)
		(else ((get 'deriv (operator exp))
			   (operands exp)
			   var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;;(2)
(define (install-sum-deriv)
  (define (deriv exp var)
	(make-sum (deriv (addend exp) var)
			  (deriv (augend exp) var)))
  (put 'deriv '(sum) deriv)
  'done)
(define (install-product-deriv)
  (define (deriv exp var)
	(make-sum
	  (make-product (multiplier exp)
					(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
					(multiplicand exp))))
  (put 'deriv '(product) deriv)
  'done)
;;(3)
(define (install-exponent-deriv)
  (define (deriv exp var)
	(let ((exponent (exponential exp))
		  (pow (power exp)))
	  (make-product
		pow
		(make-expt exponent (make-minor pow 1))
		(deriv exponent var))))
  (put 'deriv '(exponent) deriv)
  'done)
;;(4)
;;deriv 函数的定义不必改变，在put时保持与get一致即可。


;;++++++++++++++++++++++++++++++++++++++exercise 2.74++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (put <op> <type> <item>)
;; (get <op> <type>)
;; 空着，看答案
;;
;;
;;
;;
;;
;;
;;++++++++++++++++++++++++++++++++++++++exercise 2.75++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (put <op> <type> <item>)
;; (get <op> <type>)
(define (make-from-real-imag x y)
  (define (dispatch op)
	(cond ((eq? op 'real-part) x)
		  ((eq? op 'imag-part) y)
		  ((eq? op 'magnitude)
		   (sqrt (+ (square x) (square y))))
		  ((eq? op 'angle) (atan y x))
		  (else
			error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
(define (apply-generic op arg) (arg op))
(define (make-from-mag-ang r a)
  (define (dispatch op)
	(cond ((eq? op 'magnitude) r)
		  ((eq? op 'angle) a)
		  ((eq? op 'real-part) (* r (cos a)))
		  ((eq? op 'imag-part) (* r (sin a)))
		  (else
			error "Unknow op -- MAKE-FROM-MAG-ANG" op))))


;;++++++++++++++++++++++++++++++++exercise 2.76+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;加入新类型时：
;;		显式分派：各种通用型操作都要完全重写
;;		数据导向：对该类型定义相应的操作函数并put即可，已经定义的部分不必修改
;;		消息传递：新定义该类型即可
;;加入新操作是：
;;		显式分派：所有类型都要重新定义一遍
;;		数据导向：所有类型都要重新定义一遍
;;		消息传递：所有类型重新定义一遍
