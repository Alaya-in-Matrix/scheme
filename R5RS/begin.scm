(define-syntax mybegin
  (syntax-rules ()
                ((mybegin expr) expr)
                ((mybegin expr1 expr2 ...)
                 (let ((x expr1))
                   (mybegin expr2 ...)))))


(define begin-func
  (lambda (expr1 expr2)
    ((lambda (x) expr2) expr1)))


(define x 3)
(define y 3)
(define z 3)
(mybegin
  (set! x 4)
  (set! x 5))
(begin-func
  (set! y 4)
  (set! y 5))
((lambda (expr1 expr2) ((lambda (x) expr2) expr1)) 
 (set! z 4)
 (set! z 5))
;;don't know why
;;y=4 in MIT-SCHEME
;;y=5 in racket
(define-syntax swap
  (syntax-rules ()
                ((swap x y)
                 (let ((tmp x))
                   (begin 
                     (set! x y)
                     (set! y tmp))))))
(define-syntax rotate
  (syntax-rules ()
                ((rotate a b) (swap a b))
                ((rotate a b c) 
                 (begin 
                   (swap a b)
                   (swap b c)))))

(define-syntax mydo
  (syntax-rules (unless)
                ((mydo a) a)
                ((mydo a unless b) 
                 (if (not b)
                   a))))
