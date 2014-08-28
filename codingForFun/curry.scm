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
        (error "errMsg: I don't wan't to handle procedure with variable-length argument list -- CURRY")
        (curry-aux f '() (procedure-arity-min (procedure-arity f)))))))

;;simple macro to do auto-currying
;;just learnt some cases of macro days ago
;;I don't know whether this is the right way to do so
(define-syntax curry-def
  (syntax-rules ()
                ((curry-def (f x y) expr)
                 (define f (curry (lambda (x y) expr))))
                ((curry-def f expr)
                 (begin (define f expr)
                        (if (procedure? f)
                            (begin (set! f (curry f)) f)
                            f)))))
;; test case
(define (mult a b)  
        (* a b))
(curry-def (curry-mult a b) 
           (* a b))
(define add 
        (lambda (x y) (+ x y)))
(curry-def curry-add
           (lambda (x y) (+ x y)))
(mult 3 4) ;;12
((curry-mult 3) 4);;12
(add 3 4);;7
((curry-add 3) 4);;7
(procedure? (curry-mult 3));;true
