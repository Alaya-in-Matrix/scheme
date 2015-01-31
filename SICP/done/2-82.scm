(load "sicp.scm")
(install-package)

;;++++++++++++++++++++++++++++++++++++相关数据+++++++++++++++
(define n (make-scheme-number 3));;3
(define r (make-rational 3 1));;3/4
(define c (make-complex-from-real-imag 3 0));; 3+4i
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;;+++++++++++++三参数函数+++++++++++++++++++++++++++++++++++
(define (install-equ-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  (define (tri-equ? c1 c2 c3)
	(and (= (real-part c1) (real-part c2) (real-part c3))
		 (= (imag-part c1) (imag-part c2) (imag-part c3))))
  (put 'tri-equ? '(complex complex complex) tri-equ?))
(install-equ-complex-package)
(define (tri-equ? x y z)
  (apply-generic 'tri-equ? x y z))
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++





;;++++++++++++有理数/整数->复数的强制转换安装包+++++++++++++++++++++
(define (coercion-install)
  (define (scheme-number->rational n)
	(make-rational n 1))
  (define (scheme-number->complex n)
	(make-complex-from-real-imag (contents n) 0))
  (define (rational->complex r)
	(make-complex-from-real-imag  (/ (numer r) (denom r))0))
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put-coercion 'rational 'complex rational->complex))
(coercion-install)
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



;;++++++++++++++++++++++++++测试程序+++++++++++++++++++++++++++++
