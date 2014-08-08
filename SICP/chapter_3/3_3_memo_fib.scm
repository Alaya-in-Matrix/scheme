;;3.3.3 Representation of Tables
(define false #f)
(define (lookup key table)
  (let ((rec (assoc key (cdr table))))
    (if rec
      (cdr rec)
      false)))
(define (insert! key value table)
  (let ((rec (assoc key (cdr table))))
    (if rec
        (set-cdr! rec value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))
(define (memorize f)
  (let ((local-table (make-table)))
    (lambda (x)
      (let ((record (lookup x local-table)))
        (if record
            record
            (let ((result (f x)))
              (insert! x result local-table)
              result))))))
(define memo-fib
  (memorize 
   (lambda (n)
     (cond ((<= n 0) 0)
           ((= n 1) 1)
           (else
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))
(memo-fib 2)
