(load "ch2support.scm")
(load "complex_package.scm")
(load "types.scm")
(load "coercion.scm");
(load "common-define.scm");
;;=================================================
(define (install)
  (install-raise)
  (install-polar-package)
  (install-rectangular-package)
  (install-complex-package)
  (install-rational-package)
  (install-scheme-number-package))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))

;;整数包
(define (install-scheme-number-package)
  (define (tag x)
	(attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
	   (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
	   (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
	   (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
	   (lambda (x y) (/ x y)))
  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  ;;============================================
  (put '=zero? '(scheme-number)
	   (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
	   (lambda (x y) (tag (expt x y))))
  ;;============================================
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;;有理数包
(define (install-rational-package)
  ;;internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g))))
  (define (add-rat x y)
	(make-rat
	  (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	  (* (denom x) (denom y))))
  (define (sub-rat x y)
	(make-rat
	  (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	  (* (denom x) (denom y))))
  (define (mul-rat x y)
	(make-rat
	  (* (numer x) (numer y))
	  (* (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat 
	  (* (numer x) (denom y))
	  (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  ;;=========================================
  (put '=zero? '(rational)
	   (lambda (r) (= (numer r) 0)))
  ;;========================================
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))


;;复数包
(define (install-complex-package)
  ;;my_code
  ;;(install-rectangular-package)
  ;;(install-polar-package)
  ;;imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))
  

  ;;internal procedures
  (define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
					   (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					   (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	   (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
	   (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
	   (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
	   (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
	   (lambda (x y ) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  ;;+++++++++++++++++++++++++++++++++
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;;++++++++++++++++++++++++++++++++
  (put '=zero? '(complex)
	   (lambda (z) (= (magnitude z) 0)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;;2.78 
(define (attach-tag type content)
  (if (number? content)
	content
	(cons type content)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
		((number? datum) 'scheme-number)
		(else (error "Bad typed datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
		((number? datum) datum)
		(else (error "Bad typed datum -- CONTENTS" datum))))

;;2.79
(define (equ? x1 x2)
  (let ((sub_result (sub x1 x2)))
	(=zero? sub_result)))
;;2.80
(define (=zero? num) (apply-generic '=zero? num))



(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		(apply proc (map contents args))
		(if (= (length args) 2)			;;目前只能支持两个参数的问题
			(let ((type1 (car type-tags))
				  (type2 (cadr type-tags))
				  (a1	 (car args))
				  (a2	 (cadr args)))
			  (let ((t1->t2 (get-coercion type1 type2))
					(t2->t1 (get-coercion type2 type1)))
				(cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
					  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
					  (else
						(error "No method for these types"
							   (list op type-tags))))))
			(error "No method for these types"
				   (list op type-tags)))))))



;;2.81 
;;(a) 会死循环
;;(b) 会死循环,无法正常工作
;;(c)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		(apply proc (map contents args))
		(if (= (length args) 2)			;;目前只能支持两个参数的问题
		  (let ((type1 (car type-tags))
				(type2 (cadr type-tags)))
			(if (not (equal? type1 type2))
			  (let ((a1 (car args))
					(a2 (cadr args))
					(t1->t2 (get-coercion type1 type2))
					(t2->t1 (get-coercion type2 type1)))
				(cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
					  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
					  (else
						(error "No method for these types"
							   (list op type-tags)))))
			  (error "No method for these types"
					 (list op type-tags))))
		  (error "No method for these fucking types"
				 (list op type-tags)))))))


;; 2.82
;; 应该找到共同的基类?

;;2.83
;;(define (raise num) (apply-generic 'raise num)) 可以这样定义,然后相应的过程在相关的包中定义
;;但是,2.84要求用raise重新定义apply-generic,其实也不会造成任何问题,但觉得这是不好的style
(define (install-raise)
  (put 'raise '(scheme-number)
	   (lambda (n) (make-rational n 1)))
  (put 'raise '(rational)
	   (lambda (r) (make-complex-from-real-imag (/ (numer r) (denom r)) 0))))
(define (raise x)
  (let ((proc (get 'raise (list (type-tag x)))))
	(proc x)))


;;2.84
(define (apply-generic op . args)
  (define (coerce-into s t)
	(let ((type_s (type-tag s))
		  (type_t (type-tag t)))
	  (cond ((equal? type_s type_t) s)
			(else
			  (if (not (get 'raise type_s))
				#f
				(coerce-into (raise s) t))))))
  
  (let ((type_tags (map type-tag args)))
	(let ((proc (get op type_tags)))
	  (if proc
		(apply proc (map contents args))
		(if (= (length args) 2)
		  (let ((type_1 (car type_tags))
				(type_2 (cadr type_tags)))
			(if (not (equal? type_1 type_2))
			  (let ((a1 (car args))
					(a2 (cadr args)))
				(cond ((coerce-into a1 a2)
					   (apply-generic op (coerce-into a1 a2) a2))
					  ((coerce-into a2 a1)
					   (apply-generic op a1 (coerce-into a2 a1)))
					  (else
						(error "No method for these types"
							   (list op type_tags)))))
			  (error "No method for these types"
					 (list op type_tags))))
		  (error "No method for these types"
				 (list op type_tags)))))))
