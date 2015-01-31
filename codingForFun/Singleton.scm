(define nil '())
(define Singleto
  (let ((instance nil))
    (lambda ()
      (if (null? instance)
          (let ((a 0))
            (define (dispatch msg)
              (cond ((eq? msg 'get)
                     (lambda () a))
                    ((eq? msg 'set)
                     (lambda (v) 
                       (begin (set! a v)
                              'ok)))))
              (set! instance dispatch)))
      instance)))
(define a (Singleton))
(define b (Singleton))
(eq? a b);;#t
((a 'set) 3)
((b 'get));;3

