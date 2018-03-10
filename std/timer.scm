(define (print-time ms)
  (print ms)
  (let ((seconds (quotient ms 1000))
        (ms (remainder ms 1000)))
   (print `(took ,seconds seconds and ,ms ms))))

(define (time-thunk thunk)
  (let* ((t1 (timer))
         (res (thunk))
         (t2 (timer)))
    (print-time (- t2 t1))
    res))

