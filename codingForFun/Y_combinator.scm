;;2013/11/29
;;Author:Lv Wenlong
(define (Fact-maker f)
  (lambda (n)
	(cond ((= n 0) 1)
		  (else (* n (f (- n 1)))))))

(define (fib-maker f)
  (lambda (n)
	(cond ((or (= n 0) (= n 1)) 1)
		  (else
			(+ (f (- n 1))
			   (f (- n 2)))))))
(define (Y F)
  ((lambda (procedure)
	 (F (lambda (x) ((procedure procedure) x))))
   (lambda (procedure)
	 (F (lambda (x) ((procedure procedure) x))))))
