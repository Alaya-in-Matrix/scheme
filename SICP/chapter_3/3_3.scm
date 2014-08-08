(define nil '())
;;---------------------------------------

;;3.16
;;(define (count-pairs x)
;;  (if (not (pair? x))
;;    0
;;    (+ 1
;;       (count-pairs (car x))
;;       (count-pairs (cdr x)))))
;;
;;
(define l3 (list 'a 'b 'c))

(define c7 (cons 'a 'b))
(define cc7 (cons c7 c7))
(define l7 (cons cc7 cc7))
(define c (cons 1 2))
(define cc (cons c c))
(define l4 (cons cc 4))


;;3.17
(define (member?  ele lis) ;;O(length(lis))
  (cond ((null? lis) #f)
        ((eq? ele (car lis)) #t)
        (else
          (member? ele (cdr lis)))))
(define (make-check his-init)
  (lambda (x)
    (if (member? x his-init) 
      #t 
      (begin 
        (set! his-init (append (list x) his-init))
        #f))))
(define check-hist? (make-check nil))
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (if (check-hist? x) 0 1)
       (count-pairs (car x))
       (count-pairs (cdr x)))))

;;3.18
;;(define check-loop? (make-check nil))
;;(define (loop? x)
;;  (cond ((not (pair? x)) #f)
;;        ((check-loop? x) #t)
;;        (else (loop? (cdr x)))))
(define (make-cycle x)
  (begin 
    (set-cdr! (last-pair x) x)
    x))
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))


;;3.19
(define (loop? x)
  (define (safe-cdr x)
    (cond ((pair? x) (cdr x))
          (else x)))
  (define (loop-iter? pa pb counter)
    (cond 
      ((= counter 2) #t)
      ((null? pb) #f)
      ((eq? pa pb) (loop-iter? (safe-cdr pa) (safe-cdr (safe-cdr pb)) (+ counter 1)))
      (else (loop-iter? (safe-cdr pa) (safe-cdr (safe-cdr pb)) counter))))
  (loop-iter? x x 0))



(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
;;---------------------------------------------------------
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons nil nil))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (display new-pair)
    (newline)
    (cond ((empty-queue? queue)
           (begin
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue))
          (else
            (begin
              (set-cdr! (rear-ptr queue) new-pair)
              (set-rear-ptr! queue new-pair)
              queue)))))
(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "DELETE! called with and empty queue" queue)
    (set-front-ptr! queue (cdr (front-ptr queue)))))

;;3.21
(define (print-queue q)
  (let ((l (car q)))
    (if (loop? q)
      (error "loop q!")
      l)))
;;3.22
(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (dispatch cmd)
      (cond ((eq? cmd 'empty-queue?)
             (if (null? front-ptr) #t #f))
            ((eq? cmd 'front-queue)
             (if (null? front-ptr) 
               (error "FRONT called with empty queue")
               (car front-ptr)))
            ((eq? cmd 'print-queue)
             (if (loop? front-ptr)
               (error "LOOP found when calling print-queue")
               front-ptr))
            ((eq? cmd 'insert-queue!)
             (lambda (item)
               (let ((new-pair (list item)))
                 (begin (if (null? front-ptr)
                          (begin (set! front-ptr new-pair)
                                 (set! rear-ptr  new-pair))
                          (begin (set-cdr! rear-ptr new-pair)
                                 (set! rear-ptr new-pair)))
                        front-ptr))))
            (else
              (error "unknown command"))))
    dispatch))
;;3.23
(define (make-dequeue)
  (define (make-node value) (list nil nil value))
  (define (former-node node) (car node))
  (define (next-node node) (cadr node))
  (define (value-node node) (caddr node))
  (define (set-former! node value) (set-car! node value))
  (define (set-next! node value) (set-car! (cdr node) value))
  ;;----------------------------------------------
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (dispatch cmd)
      (cond ((eq? cmd  'empty-dqueue?)
             (null? front-ptr))
            ((eq? cmd 'print-dqueue)
             (let ((iterator front-ptr))
               (define (print-iterator it)
                 (if (null? it)
                   (begin 
                     (newline))
                   (begin 
                     (display (value-node it))
                     (display " ")
                     (print-iterator (next-node it)))))
               (print-iterator iterator)))
            ((eq? cmd 'front-dqueue)
             (if (null? front-ptr)
               (error "FRONT called for an empty dqueue")
               (dispatch 'print-dqueue)))
            ((eq? cmd 'rear-dqueue)
             (if (null? front-ptr)
               (error "REAR called for an empty dqueue")
               (dispatch 'print-dqueue)))
            ((eq? cmd 'front-insert-dqueue!)
             (lambda (value)
               (let ((node (make-node value)))
                 (begin (if (null? front-ptr)
                          (begin
                            (set! front-ptr node)
                            (set! rear-ptr node))
                          (begin
                            (set-next! node front-ptr)
                            (set-front-ptr! front-ptr node)
                            (set! front-ptr node)))
                        (dispatch 'print-dqueue)))))
            ((eq? cmd 'rear-insert-dqueue!)
             (lambda (value)
               (let ((node (make-node value)))
                 (begin (if (null? front-ptr)
                          (begin 
                            (set! front-ptr node)
                            (set! rear-ptr node))
                          (begin
                            (set-next! rear-ptr node)
                            (set-former! node rear-ptr)
                            (set! rear-ptr node)))
                        (dispatch 'print-dqueue)))))
            ((eq? cmd 'front-delete-dqueue!)
             (if (null? front-ptr)
               (error "FRONT-DELETE called for an empty dqueue")
               (let ((f front-ptr))
                 (begin 
                   (set! front-ptr (next-node front-ptr))
                   (set-former! front-ptr nil)
                   (set-next! f nil) ;;I suppose that f shall be garbage-collected
                   (dispatch 'print-dqueue)))))
            ((eq? cmd 'rear-delete-dqueue!)
             (if (null? front-ptr)
               (error "REAR-DELETE called for an empty dqueue")
               (let ((r rear-ptr))
                 (begin
                   (set! rear-ptr (former-node rear-ptr))
                   (set-next! rear-ptr nil)
                   (set-former! r nil)
                   (dispatch 'print-dqueue)))))
            (else
              (error "unknown method called for a dqueue"))))
    dispatch))
(define dq (make-dequeue))

;;3.3.3 Representation of Tables
(define false #f)
(define (lookup key table)
  (let ((rec (assoc key (cdr table))))
    (if rec
      (cdr rec)
      false)))
(define (insert! key value table)
  (let ((rec (assoc key (cdr table))))
    (if rec
        (set-cdr! rec value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))
(define (memorize f)
  (let ((local-table (make-table)))
    (lambda (x)
      (let ((record (lookup x local-table)))
        (if record
            (begin
              record)
            (begin
              (display local-table)
              (insert! x (f x) local-table)))))))
(define memo-fib
  (memorize (lambda (x)
              (cond ((<= x 1) 1)
                    (else
                      (+ (memo-fib (- x 1))
                         (memo-fib (- x 2))))))))

#|
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((rec (assoc key-2 (cdr subtable))))
          (if rec (cdr rec) false))
        false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable 
        (let ((rec (assoc key-2 (cdr subtable))))
          (if rec
              (set-cdr! rec value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)
|#
