(define-syntax mybegin
  (syntax-rules ()
                ((mybegin expr) expr)
                ((mybegin expr1 expr2 ...)
                 (let ((x expr1))
                   (mybegin expr2 ...)))))

(define-syntax unless
  (syntax-rules ()
                ((unless test exe)
                 (if (not test)
                     exe))
                ((unless test exe ...)
                 (if (not test)
                     (mybegin
                       exe 
                       ...)))))
(unless #f
  (newline)
  (display 1)
  (newline)
  (display 2)
  (newline)
  (display 3)
  (newline))

