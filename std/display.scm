;; defines DISPLAY in terms of primitive %DISPLAY
;; also defines WRITE and PRINT

(define (display-symbol port form) (%display port form))

(define (display-char port ch) (%display port ch))
(define (display-chars port chrs) (for-each (lambda (ch) (display-char port ch)) chrs))

(define (display-boolean port form)
  (if form
      (display-chars port '(#\# #\t))
      (display-chars port '(#\# #\f))))

(define (display-int port form) (%display port form))

(define (display-aux port readable? form)
  (cond ((symbol? form) (display-symbol port form))
	((string? form)
	 (when readable?
	   (display-char port #\"))
	 (%display port form) ;; TODO escaping
	 (when readable?
	   (display-char port #\")))
        ((char? form)
	 (when readable?
	   (display-chars port '(#\# #\\)))
	 (display-char port form)) ;; TODO newlien space etc.
        ((boolean? form) (display-boolean port form))
        ((number? form) (display-int port form))
        ((null? form) (display-chars port '(#\( #\))))
	((vector? form)
	 (display-char port #\#)
	 (display-aux port readable? (vector->list form)))
	((pair? form)
	 (display-char port #\()
	 (let loop ((form form))
	   (display-aux port readable? (car form))
	   (cond ((null? (cdr form))
		  (display-char port #\)))
		 ((pair? (cdr form))
		  (display-char port #\space)
		  (loop (cdr form)))
		 (else (display-chars port '(#\space #\. #\space))
		       (display-aux port readable? (cdr form))
		       (display-char port #\))))))
	(else
	 (%display #f "[????]"))))

(define (newline) (%newline #f))

(define (display p) (display-aux #f #f p))
(define (write p) (display-aux #f #t p))
(define (print p) (display-aux #f #t p) (newline))

(define (display:port p port) (display-aux port #f p))
(define (write:port p port) (display-aux port #t p))
(define (print:port p port) (display-aux port #t p) (%newline port))
