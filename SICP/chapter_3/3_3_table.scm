;;common definition
(define nil '())
(define false #f)
(define sicp-assoc
  (lambda (key records)
    (define same-key? equal?)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else
            (assoc key (cdr records))))))
;;--------------------------------------------------------------
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (sicp-assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (sicp-assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (sicp-assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (sicp-assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch msg)
      (cond ((eq? msg 'lookup-proc) lookup)
            ((eq? msg 'insert-proc!) insert!)
            (else 
              (error "Unknown operation -- TABLE" msg))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
