;; mutual recursion
; (define (even? n)
;   (cond ((= n 0) #t)
;         (else (odd? (- n 1)))))
; (define (odd? n)
;   (cond ((= n 0) #f)
;         (else (even? (- n 1)))))

(define (even-maker? self other num)
  (cond ((= num 0) #t)
        (else (other other self (- num 1)))))
(define (odd-maker? self other num)
  (cond ((= num 0) #f)
        (else (other other self (- num 1)))))

(define (even? n)
  (even-maker? even-maker? odd-maker? n))
