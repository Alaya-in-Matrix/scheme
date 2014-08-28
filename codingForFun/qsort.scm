(define q_sort
  (lambda (cmp? lis)
    (if (or (null? lis) (null? (cdr lis)))
      lis
      (let* ((mid-value (car lis))
             (rest-lis (cdr lis))
             (left 
               (filter (lambda (x) (cmp? x mid-value)) rest-lis))
             (right 
               (filter (lambda (x) (not (cmp? x mid-value))) rest-lis)))
        (append (q_sort cmp? left)
                (cons mid-value (q_sort cmp? right)))))))
(newline)
(display (q_sort < '(3 1 4 1 5 9 2 6 5 3)))
(newline)

