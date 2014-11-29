(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch msg)
      (cond ((eq? msg 'get-signal) signal-value)
            ((eq? msg 'set-signal!) set-my-signal!)
            ((eq? msg 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" msg))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " new-value = ")
                 (display (get-signal wire))
                 (newline)
                 (display ("--------------------"))
                 (newline))))
