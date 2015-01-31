;;implementing syntax of the script for game AGE OF EMPIRES
(define-syntax define-rules
  (syntax-rules (=>)
                ((define-rules condition => action ... )
                 (if condition
                   (begin action ...)))))
