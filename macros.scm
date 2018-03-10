;; auxilary functions
(define (foldr f a l)
  (if (null? l)
      a
      (f (car l)
	 (foldr f a (cdr l)))))

(define (quasiquote^ t)
  (if (pair? t)
      (if (eq? (car t) 'unquote)
	  (cadr t)
	  (cons 'cons 
		(cons (quasiquote^ (car t))
		      (cons (quasiquote^ (cdr t))
			    '()))))
      (cons 'quote (cons t '()))))

(defmacro quasiquote
  (lambda (t)
    (quasiquote^ (cadr t))))

;; macros
(defmacro or
  (lambda (t)
    (if (null? (cdr t))
	#f
	(if (null? (cddr t))
	    (cadr t)
	    (let ((a (cadr t))
		  (b (cddr t))
		  (tmp (gensym "tmp")))
	      `(let ((,tmp ,a))
		 (if ,tmp ,tmp (or . ,b))))))))

(defmacro and
  (lambda (t)
    (if (null? (cdr t))
	#t
	(if (null? (cddr t))
	    (cadr t)
	    (let ((a (cadr t))
		  (b (cddr t)))
	      `(if ,a (and . ,b) #f))))))


;; SCHEME MACRO SHAPES
(define (cond/0? exp)
  (and (pair? exp) (eq? 'cond (car exp)) (null? (cdr exp))))
(define (cond/else? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))
       (eq? 'else (caadr exp))
       (null? (cddr exp))))
(define (cond/else-get-else exp) (cdadr exp))
(define (cond/1? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))
       (null? (cdadr exp))))
(define (cond/1-get-one exp) (caadr exp))
(define (cond/1-get-next exp) (cddr exp))
(define (cond/=>? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))
       (pair? (cdadr exp))
       (eq? '=> (cadadr exp))
       (pair? (cddadr exp))
       (null? (cdddr (cadr exp)))))
(define (cond/=>-get-test exp) (caadr exp))
(define (cond/=>-get-thunk exp) (caddr (cadr exp)))
(define (cond/=>-get-next exp) (cddr exp))
(define (cond/clause? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))))
(define (cond/clause-get-test exp) (caadr exp))
(define (cond/clause-get-rest exp) (cdadr exp))
(define (cond/clause-get-next exp) (cddr exp))



(defmacro list
  (lambda (t) (foldr (lambda (a c) `(cons ,a ,c)) ''() (cdr t))))

(defmacro when
  (lambda (exp)
    (let ((test (cadr exp))
	  (body `(begin . ,(cddr exp))))
      `(if ,test
	   ,body
	   #f))))

(defmacro unless
  (lambda (exp)
    (let ((test (cadr exp))
	  (body `(begin . ,(cddr exp))))
      `(if ,test
	   #f
	   ,body))))

(define (cond-get-next exp)
  `(cond . ,(cddr exp)))

(defmacro cond
  (lambda (exp)
    (if (cond/0? exp)
	`(exit) ;; todo void
	(if (cond/else? exp)
	    `(begin . ,(cond/else-get-else exp))
	    (if (cond/1? exp)
		`(or ,(cond/1-get-one exp) ,(cond-get-next exp))
		(if (cond/=>? exp)
		    (let ((test (cond/clause-get-test exp))
			  (thunk (cond/=>-get-thunk exp))
			  (tmp (gensym "cond-tmp")))
		      `(let ((,tmp ,test))
			 (if ,tmp
			     (,thunk ,tmp)
			     ,(cond-get-next exp))))
		    (if (cond/clause? exp)
			(let ((test (cond/clause-get-test exp))
			      (rest (cond/clause-get-rest exp)))
			  `(if ,test
			       (begin . ,rest)
			       ,(cond-get-next exp)))
			(exit) ;; bad syntax
			)))))))


(defmacro vector
  (lambda (exp)
    (let ((l (length (cdr exp)))
          (tmp (gensym "tmp")))
      (letrec ((loop (lambda (i elts)
		       (if (null? elts)
			   tmp
			   `(begin
			      (vector-set! ,tmp ,i ,(car elts))
			      ,(loop (+ i 1) (cdr elts)))))))
	`(let ((,tmp (make-vector ,l #f)))
	   ,(loop 0 (cdr exp)))))))

;; <case> ::= (case <exp> <clause> (else <exp>))
;;
;; <clause> ::= ((<thing>) <exp>)

;; (case foo ((x) 1) ((y) 2) (else 3))
;; -->
;; let tmp foo
;; (if (eq? tmp 'x) 1)
;;   ...((y) 2) (else 3))

(define (else-clause? head)
  (and (pair? head)
       (eq? 'else (car head))))

(define (length-1? lst)
 (if (null? lst) #f (if (null? (cdr lst)) #t #f)))

(define (length-2? lst)
 (if (null? lst) #f (length-1? (cdr lst))))

(define (compile-case t clauses)
  (if (null? clauses)
      '(exit)
      (let ((head (car clauses))
            (rest (cdr clauses)))
        (if (else-clause? head)
            (cadr head) ;; TODO: else needs implicit begin
            (let ((test (car head))
                  (body (cdr head)))
              `(if ,(if (length-1? test)
                        `(equal? ,t ',(car test))
                        (if (length-2? test)
                            `(or (equal? ,t ',(car test))
                                 (equal? ,t ',(cadr test)))
                            `(member ,t ',test)))
                   (begin . ,body)
                   ,(compile-case t rest)))))))

(defmacro case
  (lambda (exp)
    (let ((discriminant (cadr exp))
          (tmp (gensym "tmp")))
      `(let ((,tmp ,discriminant))
         ,(compile-case tmp (cddr exp))))))

(defmacro mapply
  (lambda (exp)
    ;;(mapply f xs arg ...)
    (let ((f (cadr exp))
	  (xs (caddr exp))
	  (args (cdddr exp))
	  (x (gensym "x")))
      `(map (lambda (,x) (,f ,x . ,args)) ,xs))))

(defmacro inc!
  (lambda (form)
    (let ((x (cadr form)))
      `(set-box! ,x (+ (unbox ,x) 1)))))

(defmacro dec!
  (lambda (form)
    (let ((x (cadr form)))
      `(set-box! ,x (- (unbox ,x) 1)))))


(defmacro time
 (lambda (t)
  `(time-thunk (lambda () ,(cadr t)))))


