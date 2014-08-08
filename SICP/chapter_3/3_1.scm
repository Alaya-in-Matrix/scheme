(load "ch3support.scm")
(define nil '())
(define square (lambda (x) (* x x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-withdraw balance)
  (lambda (amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "����")))
(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

;;3.1
(define (make-accumulator init)
  (lambda (data)
	(begin (set! init (+ data init))
		   init)))

;;3.2
(define (make-monitored f)
  (define A (make-accumulator 0))
  (lambda (x)
	(cond ((eq? x 'how-many-calls?) (A 0))	;;�����ѯ��������ô���
		  (else
			(begin (A 1) (f x))))))

;;3.3
(define (make-account balance password)
  (define (withdraw amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "����"))
  (define (deposit amount)
	(begin (set! balance (+ balance amount))
		   balance))
  (define (dispatch m p)
	(if (eq? p password)
	  (cond ((eq? m 'withdraw) withdraw) 
			((eq? m 'deposit) deposit) 
			(else (error "δ֪���� -- MAKE ACCOUNT" m)))
	  (lambda (x) "�������")))		;;Ҳ������������?
  dispatch)

;;3.4
(define (make-account balance password)
  (define counter (make-accumulator 0))
  (define (withdraw amount)
	(if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "����"))
  (define (deposit amount)
	(begin (set! balance (+ balance amount))
		   balance))
  (define (dispatch m p)
	(if (eq? p password)
	  (cond ((eq? m 'withdraw) withdraw) 
			((eq? m 'deposit) deposit) 
			(else (error "δ֪���� -- MAKE ACCOUNT" m)))
	  (lambda (x)
		(if (= (counter 1) 3) "110 !!!!!"
		  "�������"))))
  dispatch)

(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1) (+ trials-passed 1)))
		  (else
			(iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


;;3.5 ��
;;3.6
(define rand 
  (let ((x 0))	;;Ĭ�ϴ�0��ʼ
	(lambda (op)
	  (cond ((eq? op 'generate)
			 (begin (set! x (rand-update x))
					x))
			((eq? op 'reset)
			 (lambda (new-value)
			   (set! x new-value)))
			(else
			  "unknow request")))))
;;3.7
;;ref: http://eli.thegreenplace.net/2007/09/27/sicp-sections-312-313/
(define (make-joint acc passwd new-passwd)
  (define (new-dispatch m p)
	(if (eq? p new-passwd)
	    (acc m passwd)
		(error "wrong password!")))
  new-dispatch)

;;3.8
;;accumulator�мӷ����ɳ˷��ͺ�.
