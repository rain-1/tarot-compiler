(define (box x) (vector 'box x))
(define (box? x)
  (and (vector? x)
       (= 2 (vector-length x))
       (eq? 'box (vector-ref x 0))))
(define (unbox x)
  ;; TODO: check its a box
  (vector-ref x 1))
(define (set-box! x v)
  ;; TODO: check its a box
  (vector-set! x 1 v))
(define (push-box! b val)
 (set-box! b (cons val (unbox b))))
