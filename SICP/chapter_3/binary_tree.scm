(define nil '())
(define (make-node entry left right) (list entry left right))
(define (empty-node? node) (null? node))
(define (entry-node node) (car node));;û�м����Ƿ�Ϊ��node
(define (left-branch-node node) (cadr node))
(define (right-branch-node node) (caddr node))
(define (init-tree entry)
  (cons '*head* (make-node entry nil nil)))
(define (root tree) (cdr tree))
(define (empty-tree? tree) (empty-node? (root tree)))
(define (make-empty-tree) (list '*head*))
;;--------------------------------------------------------------------------
(define (insert! rec tree)
  (define (insert-root! rec root)
    (let ((ent-value (entry-node root))
          (left-node (left-branch-node root))
          (right-node (right-branch-node root)))
      (cond ((< rec ent-value)
             (if (empty-node? left-node)
                 (set-car! (cdr root) (make-node rec nil nil))
                 (insert-root! rec left-node)))
            ((> rec ent-value)
             (if (empty-node? right-node)
                 (set-car! (cddr root) (make-node rec nil nil))
                 (insert-root! rec right-node))))))
  (if (empty-tree? tree)
      (set-cdr! tree (make-node rec nil nil))
      (insert-root! rec (root tree))))
