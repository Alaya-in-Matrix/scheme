(load "ch2support.scm")
(load "complex_package.scm")
(load "types.scm")
(load "coercion.scm");
(load "common-define.scm");
;;=================================================
(define (install)
  (install-polynomial-package)
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
  (put 'neg '(scheme-number)
	   (lambda (x) (* x -1)))
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
	(let ((g (abs (gcd n d))))
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
  (put 'neg '(rational)
	   (lambda (r) (tag (make-rat (* -1 (numer r)) 
								  (denom r)))))
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
  (put 'neg '(complex)
	   (lambda (z) (tag (make-from-real-imag (* -1 (real-part z))
											 (* -1 (imag-part z))))))
  ;;++++++++++++++++++++++++++++++++
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
	(if proc
	  (proc x)
	  #f)))


;;2.84
(define (apply-generic op . args)
  (define (coerce-into s t)
	(let ((type_s (type-tag s))
		  (type_t (type-tag t)))
	  (cond ((equal? type_s type_t) s)
			(else
			  (if (not (get 'raise (list type_s)))
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


;;2.5.3
(define (install-polynomial-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list)
	(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-terms L1 L2)
	(cond ((empty-termlist? L1) L2)
		  ((empty-termlist? L2) L1)
		  (else
			(let ((t1 (first-term L1))
				  (t2 (first-term L2)))
			  (cond ((> (order t1) (order t2))
					 (adjoin-term t1 
								  (add-terms (rest-terms L1) L2)))
					((< (order t1) (order t2))
					 (adjoin-term t2
								  (add-terms L1 (rest-terms L2))))
					(else
					  (adjoin-term 
						(make-term (order t1)
								   (add (coeff t1) (coeff t2)))
						(add-terms (rest-terms L1)
								   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
	(if (empty-termlist? L1)
	    (the-empty-termlist)
		(add-terms (mul-term-by-all-terms (first-term L1) L2)
				   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
	(if (empty-termlist? L)
	    (the-empty-termlist)
		(adjoin-term
		  (make-term (+ (order t1) (order (first-term L)))		;;此处似乎用"add"代替"+"也可以吧,而且通用性会好,不过课本代码如此
					 (mul (coeff t1) (coeff (first-term L))))
		  (mul-term-by-all-terms t1 (rest-terms L)))))
  (define (negate-terms L)
	(map (lambda (term)
		   (make-term (order term)
					  (neg (coeff term))))
		 L))
  (define (div-terms L1 L2)
	(if (empty-termlist? L1)
	    (list (the-empty-termlist) (the-empty-termlist))
		(let ((t1 (first-term L1))
			  (t2 (first-term L2)))
		  (if (> (order t2) (order t1))
			  (list (the-empty-termlist) L1)
			  (let ((new-term (make-term (- (order t1) (order t2))
										 (div (coeff t1) (coeff t2)))))
				(let ((rest-of-result
						(div-terms
						  (add-terms L1 (mul-term-by-all-terms new-term
															   (negate-terms L2)))
						  L2)))
				  (list (adjoin-term new-term
									 (car rest-of-result))
						(cadr rest-of-result))))))))
  (define (div-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
	    (map (lambda (L) (make-poly (variable p1) L)) 
			 (div-terms (term-list p1) 
						(term-list p2)))
		(error "Polys not in same var -- DIV-POLY"
			   (list p1 p2))))


  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)	;;要保证希望加入term-list中的term的order高于原term-list中的所有项
	(if (=zero? (coeff term)) 
	    term-list
		(cons term term-list)))


  (define (add-poly p1 p2)	;;多项式用一种称为poly的数据结构表示
    (if (same-variable? (variable p1) (variable p2))
	  (make-poly (variable p1)
			     (add-terms (term-list p1)
						    (term-list p2)))
	  (error "Ploys not in save var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2) 
	(if (same-variable? (variable p1) (variable p2)) 
	  (make-poly (variable p1) 
				 (mul-terms (term-list p1) 
							(term-list p2))) 
	  (error "Ploys not in same var -- MUL-POLY" (list p1 p2))))
  (define (tag p)
	(attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
	   (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make '(polynomial)
	   (lambda (var terms) (tag (make-poly var terms))))
  ;;=======================================================
  (put '=zero? '(polynomial)
	   (lambda (p) (empty-termlist? (term-list p))));;term为(0 0)时会出错,不过运算结果不会错

  (define (negate p)								;;这个解法丧失了通用型,只适用于termlist是用list来表示时,可以参考Eli Bendersky的解法
	(make-poly (variable p)							;;比如,当多项式用稠密多项式的表示法时,似乎就不能这样用了
			   (map (lambda (term)
					  (make-term (order term)
								 (neg (coeff term))))
					(term-list p))))
  (put 'neg '(polynomial)
	   (lambda (p)
		 (tag (negate p))))
  (put 'sub '(polynomial polynomial)
	   (lambda (p1 p2)
		 (tag (add-poly p1 (negate p2)))))
  (put 'div '(polynomial polynomial)
	   (lambda (p1 p2)
		 (map tag (div-poly p1 p2))))
  (put 'div-result '(polynomial polynomial)
	   (lambda (p1 p2)
		 (tag (car (div-poly p1 p2)))))
  (put 'p-remainder '(polynomial polynomial)
	   (lambda (p1 p2)
		 (tag (cadr (div-poly p1 p2)))))
  ;;=======================================================
  'done)
(define (make-polynomial var terms)
  ((get 'make '(polynomial)) var terms))
(define (div-result p1 p2)
  (apply-generic 'div-result p1 p2))
(define (p-remainder p1 p2)
  (apply-generic 'p-remainder p1 p2))


;;2.87
;;已经安装在install-polynomial-package中


;;2.88
(define (neg x) (apply-generic 'neg x))

;;2.89 
;;在2.90中做掉吧!
;;
;;2.90
;;不想做了怎么办

;;2.91
;;多项式除法

;;2.92

(define (coerce-poly p var)  ;;p是没有tag的polynomial,var是新的变元符号,返回以新符号为变元的项表
  (let ((now-var (variable p)))
	(if (same-varialbe? var now-var)
	    (term-list p)
		(let ((L (term-list p)))
		  (let ((t (first-term L))
				(rest (rest-terms L)))
			(let ((o (order t))
				  (c (coeff t)))
			  (let ((t1 (make-term 0
								   (make-polynomial now-var (adjoin-term (make-term o 1)
																		 (the-empty-termlist)))))
					(L1 (if (equal? (type-tag c) 'polynomial)
						    (let ((content (contents c)))
							  (if (same-variable? var (variable content))
								  (term-list content)
								  (coerce-poly content var)))
							(adjoin-term (make-term 0 c)
										 (the-empty-termlist)))))
				(add-terms (mul-term-by-all-terms t1 L1)
						   (coerce-poly (make-poly now-var rest) var)))))))))
							 
(define (add p1 p2)
  (add-poly p1 (coerce-poly p2 (variable p1))))
