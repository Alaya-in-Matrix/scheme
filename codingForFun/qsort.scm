(define q_sort
  (lambda (pred? lis)
	(cond ((<= (length lis) 1) lis)
		  (else (let ((lis_1 (filter (lambda (x) (pred?  x (car lis))) (cdr lis)))
					  (lis_2 (filter (lambda (x) (not (pred? x (car lis)))) (cdr lis))))
				  (append (q_sort pred? lis_1)
						  (list (car lis))
						  (q_sort pred? lis_2)))))))
