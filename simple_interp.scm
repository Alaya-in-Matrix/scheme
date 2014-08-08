(define calc
  (lambda (expr)
	(if (number? expr)
	    expr
		(let ((op (car expr))
			  (e1 (cadr expr))
			  (e2 (caddr expr)))
		  (let ((v1 (calc e1))
				(v2 (calc e2)))
			(cond ((eq? op '+) (+ v1 v2))
				  ((eq? op '-) (- v1 v2))
				  ((eq? op '*) (* v1 v2))
				  ((eq? op '/) (/ v1 v2))))))))
