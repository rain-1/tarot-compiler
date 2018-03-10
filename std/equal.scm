;; TODO equality of vectors

(define (equal? x y)
  (if (pair? x)
      (if (pair? y)
	  (if (equal? (car x) (car y))
	      (equal? (cdr x) (cdr y))
	      #f)
	  #f)
      (if (string? x)
          (if (string? y) 
              (string=? x y)
              #f)
          (eq? x y))))
