(define (not b) (if b #f #t))

(define (zero? x) (= x 0))
(define (even? x) (= 0 (modulo x 2)))
(define (odd? x) (not (even? x)))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (abs x) (if (negative? x) (- 0 x) x))

(define (min x y) (if (< x y) x y))
(define (max x y) (if (> x y) x y))

(define (digit->number d) (- (char->integer d) (char->integer #\0)))
(define (string->number s)
  (let ((l (string-length s)))
    (let loop ((n 0) (i 0))
      (if (= i l)
	  n
	  (let ((digit (string-ref s i)))
	    (loop (+ (* 10 n) (digit->number digit)) (+ i 1)))))))
