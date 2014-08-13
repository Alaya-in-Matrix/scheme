;;test case
(define neg (curry * -1))
(define square (curry expt nil 2))
(define my-sqrt (curry expt nil 0.5))
(define testNum -4)


(begin
  (set! testNum (neg testNum))
  (display testNum);;4
  (newline)

  (set! testNum (square testNum))
  (display testNum);;16
  (newline)

  (set! testNum (square testNum))
  (display testNum);;4
  (newline))

