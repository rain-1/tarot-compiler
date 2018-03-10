(define (eval exp)
  (let ((p (vm:open)))
    (compile exp #f (lambda (thunk)
		      (thunk p #f))))
     (vm:finish p))

