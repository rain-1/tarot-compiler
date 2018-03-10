(define (empty-queue) (list->vector (list 'queue '() #f)))

(define (queue:top q) (vector-ref q 1))
(define (queue:bot q) (vector-ref q 2))
(define (queue:top! q v) (vector-set! q 1 v))
(define (queue:bot! q v) (vector-set! q 2 v))

(define (queue-push! q v)
  (if (queue:bot q)
      (begin (set-cdr! (queue:bot q) (list v))
	     (queue:bot! q (cdr (queue:bot q))))
      (begin (queue:top! q (list v))
	     (queue:bot! q (queue:top q)))))

(define (queue-pop! q)
  (let ((top (queue:top q)))
    (if (null? top)
	(error 'queue-pop! 0 0)
	(begin
	  (queue:top! q (cdr top))
	  (when (null? (cdr top))
	    (queue:bot! q #f))
	  (car top)))))

(define (queue->list q)
  (copy-list (queue:top q)))
