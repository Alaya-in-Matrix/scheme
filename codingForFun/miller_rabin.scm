;; miller-rabin test
(define (square x) (* x x))
;;+++++++++++++++++++++++++++++++++++++++++++++++++common definition++++++++++

(define (fast-prime? num)
  (define (expmod-with-check base expo m)
    (cond ((= expo 0) 1)
          ((even? expo)
           (let ((result (expmod-with-check base (/ expo 2) m)))
             (if (and (= (remainder (square result) m) 1)
                      (not (= 1 result))
                      (not (= (- m 1) result)))
               0
               (remainder (square result) m ))))
          (else
            (remainder (* base (expmod-with-check base (- expo 1) m)) m))))
  (define (try num)
    (= 1 (expmod-with-check (+ (random (- num 1)) 1) 
                            (- num 1) 
                            num)))
  (define (try-several-times times)
    (cond ((= times 0) #t)
          ((try num)
           (try-several-times (- times 1)))
          (else #f)))
  ;;----------------------------------------------------
  (cond ((<= num 1) #f)
        (else (try-several-times 50))))

(define test_num 6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151)

