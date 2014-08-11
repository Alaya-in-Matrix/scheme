(define-syntax mybegin
  (syntax-rules ()
                ((mybegin expr) expr)
                ((mybegin expr1 expr2 ...)
                 (let ((x expr1))
                   (mybegin expr2 ...)))))


