(eval "?" 1)

;; BUILTINS

(error "<std>" 3)

(cons "<std>" 2)
(car "<std>" 1)
(cdr "<std>" 1)
(set-car! "<std>" 2)
(set-cdr! "<std>" 2)
(null? "<std>" 1)
(pair? "<std>" 1)
(symbol? "<std>" 1)
(string? "<std>" 1)
(char? "<std>" 1)
(boolean? "<std>" 1)
(number? "<std>" 1)

(%display "<std>" 2)
(%newline "<std>" 1)
(exit "<std>" 0)

; equality
(eq? "<std>" 2)
(= "<std>" 2)

; arithmetic
(* "<std>" 2)
(/ "<std>" 2)
(+ "<std>" 2)
(- "<std>" 2)
(modulo "<std>" 2)
(quotient "<std>" 2)
(remainder "<std>" 2)
(> "<std>" 2)
(< "<std>" 2)
(>= "<std>" 2)
(<= "<std>" 2)

; vector
(make-vector "<std>" 2)
(vector? "<std>" 1)
(vector-ref "<std>" 2)
(vector-set! "<std>" 3)
(vector-length "<std>" 1)

; string
(make-string "<std>" 2)
(string-length "<std>" 1)
(string-ref "<std>" 2)
(string-set! "<std>" 3)
(string->symbol "<std>" 1)
(string=? "<std>" 2)
(string-append "<std>" 2)

; io
(read-char "<std>" 1)
(peek-char "<std>" 1)
(eof-object? "<std>" 1)
(open-input-file "<std>" 1)
(open-output-file "<std>" 1)
(close-port "<std>" 1)
(standard-input "<std>" #f)
(command-line-arguments "<std>" 0)
(timer "<std>" 0)

; vm
(vm:open "<std>" 0)
(vm:finish "<std>" 1)
(display:port "<std>" 2)

(gensym "<std>" 1)

(integer->char "<std>" 1)
(char->integer "<std>" 1)
(symbol->string "<std>" 1)

;;;;;;;;;;;;;;;;;;;;

