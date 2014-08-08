(load "ch2support.scm")
(load "common-define.scm")
(load "complex_package.scm")
(load "types.scm")
(load "coercion.scm")
;;+++++++++++++++++++My_Code++++++++++++++++++++++++++++++++++++++++++++++
(define (install-package)
  (install-raise)
  (install-project)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-rectangular-package)
  (install-real)
  (install-polar-package))
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (install-real)
  (define (tag x)
	(attach-tag 'real x))
  (put 'add '(real real)
	   (lambda (x y) (+ x y)))
  (put 'sub '(real real)
	   (lambda (x y) (- x y)))
  (put 'mul '(real real)
	   (lambda (x y) (* x y)))
  (put 'div '(real real)
	   (lambda (x y) (/ x y)))
  (put 'make 'real
	   (lambda (x) (tag x)))
  'done)
(define (make-real x)
  ((get 'make 'real) x))


;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
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
  
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++exercise++++++++++++++++++++++++++++++++++++++++++++++++++
;;2.78
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
		((number? datum) 'scheme-number)
		(else (error "Bad typed datum --TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
		((number? datum) datum)
		(else (error "Bad typed datum -- CONTENTS" datum))))
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number) 
	contents
	(cons type-tag contents)))

;;2.79
(define (equ? x1 x2)
  (define numer car)
  (define denom cdr)
  (put 'equ '(scheme-number scheme-number) =)
  (put 'equ '(rational rational)
	   (lambda (r1 r2) (and (= (numer r1) (numer r2))
							(= (denom r1) (denom r2)))))
  (put 'equ '(complex complex)
	   (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
							(= (imag-part z1) (imag-part z2)))))
  (put 'equ '(real real)
	   (lambda (x1 x2)
		 (= (contents x1) (contents x2))))
  (apply-generic 'equ x1 x2))
;;2.80
(define (=zero? x)
  (define numer car)
  (put 'zero? '(scheme-number) 
	   (lambda (y) (= 0 y)))
  (put 'zero? '(rational)
	   (lambda (y) (= 0 (numer y))))
  (put 'zero? '(complex)
	   (lambda (y) (= 0 (magnitude y))))
  (apply-generic 'zero? x))



;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;(define (apply-generic op . args)
;;  (let ((type-tags (map type-tag args)))
;;    (let ((proc (get op type-tags)))
;;      (if proc
;;          (apply proc (map contents args))
;;          (if (= (length args) 2)
;;              (let ((type1 (car type-tags))
;;                    (type2 (cadr type-tags))
;;                    (a1 (car args))
;;                    (a2 (cadr args)))
;;                (let ((t1->t2 (get-coercion type1 type2))
;;                      (t2->t1 (get-coercion type2 type1)))
;;                  (cond (t1->t2
;;                         (apply-generic op (t1->t2 a1) a2))
;;                        (t2->t1
;;                         (apply-generic op a1 (t2->t1 a2)))
;;                        (else
;;                         (error "No method for these types"
;;                                (list op type-tags))))))
;;              (error "No method for these types"
;;                     (list op type-tags)))))))


;;2.81
;;(define (apply-generic op . args) ;; example (define (add x y) (apply-generic 'add x y))
;;  (let ((type-tags (map type-tag args)))
;;	(let ((proc (get op type-tags)))
;;	  (if proc
;;		  (apply proc (map contents args))
;;		  (if (= (length args) 2)
;;			(let ((type1 (car type-tags))
;;				  (type2 (cadr type-tags))
;;				  (a1 (car args))
;;				  (a2 (cadr args)))
;;			  (if (not (eq? type1 type2))
;;				  (let ((t1->t2 (get-coercion type1 type2))
;;						(t2->t1 (get-coercion type2 type1)))
;;					(cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
;;						  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
;;						  (else
;;							(error "No method for these types"
;;								   (list op type-tags)))))
;;				  (error "No method and same type"
;;						 (list op type-tags))))
;;			(error "No method fro these types"
;;				   (list op type-tags)))))))
;;
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;2.82
;;课本策略的不足:譬如我本来可以全都coerce到complex,然后就可以调用函数解决,但是实际调用时,给的参数时(scheme-number,rational,rational),用课本上的策略;;就无法可想了

;;(define (apply-generic op . args)
;;  (define type-list '(scheme-number rational complex))
;;  (define (apply-gene op type-list args)
;;	(let ((type-tags (map type-tag args)))
;;	  (let ((proc (get op type-tags)))
;;		(if proc
;;		  (apply proc (map contents args))
;;		  (if (null? type-list)
;;			(error "No Method Found!" (list op type-tags))
;;			(let ((new-proc (get op (map (lambda (x) (car type-list)) type-tags))))
;;			  (if (and new-proc (can-coerce? type-tags (car type-list)))
;;				(apply new-proc (map contents (map (lambda (arg)
;;													 (if (equal? (type-tag arg) (car type-list))
;;													   arg
;;													   ((get-coercion (type-tag arg) (car type-list)) arg)))
;;												   args)))
;;				(apply-gene op (cdr type-list) args))))))))
;;  (define (can-coerce? types target)
;;	(cond ((equal? types nil) #f)
;;		  ((= (length types) 1) (or (get-coercion (car types) target)
;;									(equal? (car types) target)))
;;		  (else
;;			(and (can-coerce? (list (car types)) target)
;;				 (can-coerce? (cdr types) target)))))
;;  (apply-gene op type-list args))

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;2.83
(define (install-raise)
  (put 'raise 'scheme-number
	   (lambda (n)
		 (make-rational n 1)))
  (put 'raise 'rational
	   (lambda (r)
		 (make-real (/ (numer r) (denom r)))))
  (put 'raise 'real
	   (lambda (x)
		 (make-complex-from-real-imag (contents x) 0))))
(define (raise num)
  (let ((raise-proc (get 'raise (type-tag num))))
	(if raise-proc
	  (raise-proc num)
	  #f)))

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;2.84

(define (test x)
  (begin
	(newline)
	(display "=======================\n")
	(display x)
	(newline)
	(display "=============================================================\n")))

(define (apply-generic op . args)
  (define (can-coerce? arg target-type)
	(let ((tag (type-tag arg)))
	  (cond ((equal? tag target-type) #t)
			((not (get 'raise tag)) #f)		;;op 已然在类型塔的top
			(else (can-coerce? (raise arg) target-type)))))
  (define (coerce arg target-type)
	  (cond ((equal? (type-tag arg) target-type) arg)
			((not (get 'raise (type-tag arg))) #f)
			(else (coerce (raise arg) target-type))))
  (define (check op args)
	(let ((op-lis (filter (lambda (x) (get op (map (lambda (y) (type-tag x)) args)))
						  args)))
	  (map-or (lambda (x) 
				(if (map-and (lambda (y)
							   (can-coerce? y (type-tag x)))
							 args)
				  (type-tag x)
				  #f))
			  op-lis)))
  (define (fuck op args)
	(let ((type-tags (map type-tag args)))
	  (test type-tags)
	  (let ((proc (get op type-tags)))
		(if proc
		  (apply proc (map contents args))
		  (let ((target-type (check op args)))
			(if target-type 
			  (fuck op (map (lambda (x) (coerce x target-type)) args))
			  (if (map-or (lambda (x) (not (get 'raise target-type))) args)
				(error "No Method Fonud --" (list 'fuck op args))
				(fuck op (map raise args)))))))))
  (fuck op args))

				
;;2.85
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define (install-project)
  (put 'project 'rational 
	   (lambda (r)
		 (make-scheme-number (round (/ (numer r) (denom r))))))
  (put 'project 'real 
	   (lambda (r)
		 (make-rational (round (mul r (make-real 10000))) 10000)));;保留四位小数
  (put 'project 'complex
	   (lambda (c)
		 (make-real (real-part c)))))
(define (project x)
  (let ((project-proc (get 'project (type-tag x))))
	(if project-proc 
	  (project-proc x)
	  #f)))
(define (drop x)
  (let ((temp (project x)))
	(if temp
	  (if (equ? x (raise temp))
		temp
		#f)
	  #f)))





