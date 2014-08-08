;;检测一个函数运行的时间.
;;Author 吕文龙
;;Date: 2013/8/28
;;test github
(define (check-time proc . parameters)
  (define (internal-proc proc parameters start_time)
	(apply proc parameters)
	(- (runtime) start_time))
  (internal-proc proc parameters (runtime)))
