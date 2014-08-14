(define (curry f)
  (define (curry-aux f args arity-counter)
    (cond ((<= arity-counter 1)
           (lambda (x)
             (apply f (reverse (cons x args)))))
          (else
            (lambda (x)
              (curry-aux f (cons x args) (- arity-counter 1))))))
  (let ((arity (procedure-arity f)))
    (let ((min-arity (procedure-arity-min arity))
          (max-arity (procedure-arity-max arity)))
      (if (not (equal? min-arity max-arity)) 
        (error "errMsg: I don't wan't to handle variable length argument list -- CURRY")
        (curry-aux f '() (procedure-arity-min (procedure-arity f)))))))

;;simple macro to do auto-currying
;;just learnt some cases of macro days ago
;;I don't know whether this is the right way to do so
(define-syntax curry-def
  (syntax-rules ()
                ((curry-def (f x y) expr)
                            (begin (define (f x y) expr)
                                   (set! f (curry f))))
                ((curry-def f expr)
                 (begin (define f expr)
                        (set! f (curry f))
                        f))))

;; test case
(curry-def sub (lambda (x y z) (- x y z)))

