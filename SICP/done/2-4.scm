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
;;deriv �����Ķ��岻�ظı䣬��putʱ������getһ�¼��ɡ�


;;++++++++++++++++++++++++++++++++++++++exercise 2.74++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (put <op> <type> <item>)
;; (get <op> <type>)
;; ���ţ�����
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
;;����������ʱ��
;;		��ʽ���ɣ�����ͨ���Ͳ�����Ҫ��ȫ��д
;;		���ݵ��򣺶Ը����Ͷ�����Ӧ�Ĳ���������put���ɣ��Ѿ�����Ĳ��ֲ����޸�
;;		��Ϣ���ݣ��¶�������ͼ���
;;�����²����ǣ�
;;		��ʽ���ɣ��������Ͷ�Ҫ���¶���һ��
;;		���ݵ����������Ͷ�Ҫ���¶���һ��
;;		��Ϣ���ݣ������������¶���һ��
