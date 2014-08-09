(define-syntax mybegin
  (syntax-rules ()
                ((mybegin expr) expr)
                ((mybegin expr1 expr2 ...)
                 (let ((x expr1))
                   (mybegin expr2 ...)))))

(define x 3)
(mybegin
  (set! x 4)
  (set! x 5)
  (set! x 6))

(define begin-func
  (lambda (expr1 expr2)
    ((lambda (x) expr2) expr1)))


(define y 3)
(begin-func
  (set! y 4)
  (set! y 5))
;;don't know why
;;y=4 in MIT-SCHEME
;;y=5 in racket

