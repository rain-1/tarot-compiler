(define (empty-stack) (box '()))

(define (make-stack lst) (box lst))

(define (stack-top s)
  (let ((stk (unbox s)))
    (if (null? stk)
	(error 'stack-top/null 0 0)
	(car stk))))

(define (stack-pop! s)
  (let ((stk (unbox s)))
    (if (null? stk)
	(error 'stack-pop/null 0 0)
	(begin
	  (set-box! s (cdr stk))
	  (car stk)))))

(define (stack-push! s v) (set-box! s (cons v (unbox s))))

(define (stack-get s) (unbox s))
