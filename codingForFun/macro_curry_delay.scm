;;learnt from somewhere on internet
(define-syntax curry-lambda
  (syntax-rules ()
                ((curry-lambda () body ...) (lambda () body ...))
                ((curry-lambda (x) body ...) (lambda (x) body ...))
                ((curry-lambda (arg args ...) body ...)
                 (lambda (arg . rest)
                   (let ((next (curry-lambda (args ...) body ...)))
                     (cond ((null? rest) next)
                           (else (apply next rest))))))))
(define-syntax cdef ;;curreid define
  (syntax-rules ()
                ((cdef (name args ...) body ...)
                 (define name (curry-lambda (args ...) body ...)))
                ((cdef name (lambda (args ...) body ...))
                 (define name (curry-lambda (args ...) body ...)))))
(define-syntax mydelay
  (syntax-rules (define)
    ((mydelay (define expr ...))
     (error "can't delay define -- macro mydelay"))
    ((mydelay expr ...)
     (cons 'promise (lambda () (begin expr ...))))))

(define-syntax myforce
  (syntax-rules ()
    ((myforce delayed-expr)
     (if (and (pair? delayed-expr) (eq? 'promise (car delayed-expr)))
       ((cdr delayed-expr))
       (error "not a promise")))))
(cdef (add x y)
      (+ x y))
(cdef sub 
      (lambda (x y) (- x y)))

