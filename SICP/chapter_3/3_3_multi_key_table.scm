(define (make-table)
  (list '*table*))
;;多维table的实现, 似乎有些臃肿且低效
(define (insert! key-list value table)
  (define (creat-table-record key-list value)
    (let ((first-key (car key-list))
          (rest-keys (cdr key-list)))
      (if (null? rest-keys)
        (cons first-key value)
        (cons first-key
              (list (creat-table-record rest-keys value))))))
  (let ((first-key (car key-list))
        (rest-keys (cdr key-list)))
    (if (null? rest-keys)
      (let ((record (assoc first-key (cdr table))))
        (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons first-key value) (cdr table)))))
      (let ((subtable (assoc first-key (cdr table))))
        (if subtable 
          (if (pair? (cdr subtable))
            (insert! rest-keys value subtable)
            (set-cdr! subtable (list (creat-table-record rest-keys value))))
          (set-cdr! table
                    (cons (creat-table-record key-list value)
                          (cdr table)))))))
  'ok)
(define (lookup key-list table)
  (let ((first-key (car key-list))
        (rest-keys (cdr key-list)))
    (let ((record (assoc first-key (cdr table))))
      (if record
        (if (null? rest-keys)
            (cdr record)
            (lookup rest-keys record))
        false))))
;;test
(define t (make-table))
(insert! (list 'b) 3 t)
(insert! (list 'b 'c 'd) 3 t)
(insert! (list 'b 'c 'e) 3 t)
(insert! (list 'b 'c 'f) 3 t)
(insert! (list 'b 'c) 3 t)
(insert! (list 'b) 3 t)
