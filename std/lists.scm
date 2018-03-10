(define (assoc key tbl)
  (if (null? tbl)
      #f
      (if (eq? key (car (car tbl)))
          (car tbl)
          (assoc key (cdr tbl)))))

(define (length l)
  (let loop ((l l) (len 0))
    (if (null? l) len (loop (cdr l) (+ len 1)))))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (map/2 f l h)
  (if (null? l)
      (if (not (null? h))
	  (error 'map/2 "lists of different length" #f)
	  '())
      (cons (f (car l) (car h))
            (map/2 f (cdr l) (cdr h)))))

(define (for-each proc l)
  (if (null? l)
      #t
      (begin (proc (car l))
	     (for-each proc (cdr l)))))

;; foldr is defined in macros.scm

(define (foldl f a xs)
  (if (null? xs)
      a
      (foldl f (f a (car xs)) (cdr xs))))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (revappend l r)
  (if (null? l)
      r
      (revappend (cdr l) (cons (car l) r))))

(define (reverse l) (revappend l '()))

(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter p (cdr l)))
          (filter p (cdr l)))))

(define (concat-map func lst)
  (if (null? lst)
      '()
      (append (func (car lst))
              (concat-map func (cdr lst)))))

(define (concatenate lists) (concat-map (lambda (i) i) lists))

(define (member elt lst)
  (if (null? lst)
      #f
      (or (equal? elt (car lst)) (member elt (cdr lst)))))

(define (copy-list l) (map (lambda (i) i) l))

(define (fold kons knil lst)
  (if (null? lst)
      knil
      (kons (car lst)
	    (fold kons knil (cdr lst)))))

(define (index nm lst)
  (let loop ((i 0) (lst lst))
    (if (null? lst)
	#f
	(if (eq? nm (car lst))
	    i
	    (loop (+ i 1) (cdr lst))))))

(define (vector->list v)
  (let ((l (vector-length v)))
    (let loop ((i 0))
      (if (= i l)
          '()
          (cons (vector-ref v i)
                (loop (+ i 1)))))))

(define (list->vector l)
 (let ((len (length l)))
   (let ((v (make-vector len #f)))
    (let loop ((i 0) (l l))
      (if (= i len)
          v
          (begin (vector-set! v i (car l))
                 (loop (+ i 1) (cdr l))))))))

(define (list->string chrs)
  (let ((l (length chrs)))
    (let ((s (make-string l #\?)))
      (let loop ((i 0) (chrs chrs))
	(if (null? chrs)
	    s
	    (begin
	      (string-set! s i (car chrs))
	      (loop (+ i 1) (cdr chrs))))))))

(define (string->list str)
  (let ((n (string-length str)))
    (let loop ((i 0))
      (if (= i n) '()
	  (cons (string-ref str i) (loop (+ i 1)))))))

(define (replicate n elt)
  (if (= n 0) '() (cons elt (replicate (- n 1) elt))))

(define (minimum xs)
  (if (pair? xs)
      (foldl min (car xs) (cdr xs))
      #f))
(define (maximum xs)
  (if (pair? xs)
      (foldl max (car xs) (cdr xs))
      #f))
(define (sum lst) (foldl + 0 lst))
