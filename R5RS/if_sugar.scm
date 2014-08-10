;;hygiene macro
;;example of r5rs
(define x 3)
(define-syntax m
  (syntax-rules ()
                ((m) x)))
(define (test-hygiene-macro)
  (let ((x 4))
    (m)));;3
;;-------------------------------------------------------------------------
(define-syntax iff
  (syntax-rules (then els endif)
                ((iff a then b endif)
                 (if a b))
                ((iff a then b els c endif)
                 (if a b c))))
(define-syntax ==>
  (syntax-rules (iif)
                ((fuck a iif b)
                 (if b a))))

(==> 3 iif (> 1 0));;3  实现类似perl的die if ($fucked)的语法

(define (fuck);;实现if...then...els..endif语法
  (let ((a 1))
    (iff (> a 0)
         then 
            (display "positive")
         els  
            (display "negative")
         endif)))
