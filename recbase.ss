;The optimizing version which is very slow
(library (recbase)
(export z s p comp primitive-rec call)
(import (scheme))

;Three basic functions

;zero function
(define z 'z)
;successor function
(define s 's)
;projection function
(define p
 (lambda (k i)
   `(p ,k ,i)))


;Two ways to make new functions

;composite
(define (comp . fs)
  `(comp ,@fs))
;primitive recursive
(define (primitive-rec g h)
  `(rec ,g ,h))

;convert the list to the lambda-expr, and use several passes to optimize it
(define (func->lambda f)
  (define (flag f)
    (define (flag~ f)
      (if (pair? f)
	  (case (car f)
	    ((p)
	     (cons
	       (string->symbol
		 (string-append
		   "p"
		   (number->string (cadr f))
		   "~"
		   (number->string (- (caddr f) 1))))
	       (cadr f)))
	    ((comp)
	     (let ((s (map flag~ (cdr f))))
	       (if (or (null? s)
		       (null? (cdr s))
		       (not (= (cdar s) (length (cdr s))))
		       (not (apply = (map cdr (cdr s)))))
		   #f
		   (cons
		     (cons
		       (string->symbol
			 (string-append
			   "c"
			   (number->string (cdadr s))))
		       (map car s))
		     (cdadr s)))))
	    ((rec)
	     (let ((s (map flag~ (cdr f))))
	       (if (or (not (= 2 (length s)))
		       (not (= (+ 2 (cdar s)) (cdadr s))))
		   #f
		   (cons
		     (cons
		       (string->symbol
			 (string-append
			   "r"
			   (number->string (+ 1 (cdar s)))))
		       (map car s))
		     (+ 1 (cdar s))))))
	    (else #f))
	  (case f
	    ((s z) (cons f 1))
	    (else #f))))
    (let ((r (flag~ f)))
      (and r (car r))))
  (define (get-symbol-last-num sym)
    (let ((lst (reverse (string->list (symbol->string sym)))))
      (let it ((r '())
	       (lst lst))
	(if (char>=? #\9 (car lst) #\0)
	    (it (cons (car lst) r) (cdr lst))
	    (string->number (list->string r))))))
  (define (get-p-first-num sym)
    (let ((lst (cdr (memq #\~ (reverse (string->list (symbol->string sym)))))))
      (let it ((r '())
	       (lst lst))
	(if (char>=? #\9 (car lst) #\0)
	    (it (cons (car lst) r) (cdr lst))
	    (string->number (list->string r))))))
  (define (range a b)
    (if (>= a b)
	'()
	(cons a (range (+ a 1) b))))
  (define seq 0)
  (define (new-seq)
    (set! seq (+ 1 seq))
    seq)
  (define (make-symbol . s)
    (let ((prefix (car s)))
      (if (null? (cdr s))
	  (lambda (number) (make-symbol prefix number))
	  (let ((number (cadr s)))
	    (string->symbol (string-append prefix (number->string number)))))))
  (define (convert f)
    (if (pair? f)
	(case (car (string->list (symbol->string (car f))))
	  ((#\r)
	   (let* ((param-cnt (get-symbol-last-num (car f)))
		  (params (map
			    (make-symbol "x")
			    (reverse
			      (fold-left
				(lambda (r f) (cons (f) r))
				'()
				(make-list param-cnt new-seq)))))
		  (r-params (reverse params))
		  (params-except-last (reverse (cdr r-params)))
		  (last-param (car r-params))
		  (func-name (make-symbol "f" (new-seq))))
	   `(letrec ((,func-name
		       (lambda ,params
			 (if (zero? ,last-param)
			     (,(convert (cadr f)) ,@params-except-last)
			     (,(convert (caddr f))
			       ,@params-except-last
			       (- ,last-param 1)
			       (,func-name ,@params-except-last (- ,last-param 1)))))))
	      ,func-name)))
	  ((#\c)
	   (let* ((param-cnt (get-symbol-last-num (car f)))
		  (params (map
			    (make-symbol "x")
			    (reverse
			      (fold-left
				(lambda (r f) (cons (f) r))
				'()
				(make-list param-cnt new-seq))))))
	   `(lambda ,params
	      (,(convert (cadr f))
		,@(map (lambda (x) (cons (convert x) params)) (cddr f)))))))
	(case f
	  ((s) '(lambda (x) (+ x 1)))
	  ((z) '(lambda (x) 0))
	  (else ;px~y
	    (let* ((param-cnt (get-p-first-num f))
		   (result-seq (get-symbol-last-num f))
		   (params (map
			     (make-symbol "x")
			     (reverse
			       (fold-left
				 (lambda (r f) (cons (f) r))
				 '()
				 (make-list param-cnt new-seq)))))
		   (result (list-ref params result-seq)))
	      `(lambda ,params ,result))))))

  ;optimization
  (define (optimize s)
    (define (find-first f lst failed-value)
      (let it ((lst lst))
	(if (null? lst)
	    failed-value
	    (if (f (car lst))
		(car lst)
		(it (cdr lst))))))
    (define (reduce f lst)
      (let it ((init (car lst))
	       (lst (cdr lst)))
	(if (null? lst)
	    init
	    (it (f init (car lst)) (cdr lst)))))
    (define (remove-alist-keys alist keys)
      (define (remove-alist-key alist key)
	(cond
	  ((null? alist) alist)
	  ((eq? (caar alist) key) (cdr alist))
	  (else (cons (car alist) (remove-alist-key (cdr alist) key)))))
      (fold-left remove-alist-key alist keys))
    (define (list-ref* lst indexs)
      (fold-left
	(lambda (r n)
	  (if (pair? n)
	      (fold-left
		(lambda (f s) (f s))
		r
		(make-list (car n) cdr))
	      (list-ref r n)))
	lst
	indexs))
    (define (index lst cond?)
      (call/cc
	(lambda (return)
	  (let next ((s lst))
	    (cond
	      ((null? s) (return -1))
	      ((cond? (car s)) 0)
	      (else (+ 1 (next (cdr s)))))))))
    (define (lambda-exp? s) (and (pair? s) (eq? (car s) 'lambda)))
    ;(count-free-symbols '(+ x0 (* x1 x1))) => '((* 1) (+ 1) (x0 1) (x1 2))
    ;(count-free-symbols '(if x1 (x1 x0) (+ x0 1))) => '((+ 1) (x0 1) (x1 2))
    ;(count-free-symbols '(+ x0 x1 y)) => '((+ 1) (x0 1) (x1 1) (y 1))
    ;(count-free-symbols '(lambda (x0 x1) (+ x0 x1 y))) => '((+ 1)(y 1))
    (define (count-free-symbols s)
      (define (merge is-sorted op a b) ;op = +/max
	(define (lt a b) (apply string<? (map (lambda (x) (symbol->string (car x))) (list a b))))
	(define (_merge < s1 s2)
	  (let it ((s1 s1)
		   (s2 s2)
		   (tmp '()))
	    (cond
	      ((null? s1)
	       (if (null? tmp)
		   s2
		   (it '() (cons (car tmp) s2) (cdr tmp))))
	      ((null? s2)
	       (if (null? tmp)
		   s1
		   (it '() (cons (car tmp) s1) (cdr tmp))))
	      ((< (car s1) (car s2))
	       (it (cdr s1) s2 (cons (car s1) tmp)))
	      (else
	       (it s1 (cdr s2) (cons (car s2) tmp))))))
	(define (uniq s)
	  (let it ((r '())
		   (s s))
	    (cond
	      ((null? s) (reverse r))
	      ((null? r) (it (cons (car s) r) (cdr s)))
	      ((eq? (caar s) (caar r)) (it (cons (list (caar r) (op (cadar s) (cadar r))) (cdr r)) (cdr s)))
	      (else (it (cons (car s) r) (cdr s))))))
	(uniq
	  (if is-sorted
	    (_merge lt a b)
	    (apply _merge
		   lt
		   (map
		     (lambda (lst)
		       (sort lt lst));Racket:(sort lst lt)
		     (list a b))))))
	(cond
	  ((pair? s)
	   (case (car s)
	     ((if)
	      (merge #t +
		(count-free-symbols (list-ref s 1))
	      (merge #t max (count-free-symbols (list-ref s 2)) (count-free-symbols (list-ref s 3)))))
	     ((letrec)
	      (remove-alist-keys
		(merge #t +
		       (count-free-symbols (cddr s))
		       (count-free-symbols (list-ref* s '(1 0 1))))
		(list (caaadr s))))
	     ((lambda)
	      (remove-alist-keys
	       (reduce (lambda (a b) (merge #t + a b)) (map count-free-symbols (cddr s)))
	       (cadr s)))
	     (else
	       (reduce (lambda (a b) (merge #t + a b)) (map count-free-symbols s)))))
	  ((symbol? s) `((,s 1)))
	  (else '())))
    (define (replace-symbols expr replace-list)
      (define (find symbol)
	(let ((r (assq symbol replace-list)))
	  (and r (cdr r))))
      (define (replace s bind-symbols)
	(cond
	  ((pair? s)
	   (if (eq? (car s) 'lambda)
	       `(lambda ,(list-ref s 1) ,(replace (list-ref s 2) (append bind-symbols (list-ref s 1))))
	       (map (lambda (x) (replace x bind-symbols)) s)))
	  (else
	    (if (and (symbol? s) (not (memq s bind-symbols)))
		(let ((r (find s)))
		  (if r r s))
		s))))
      (replace expr '()))

    ;an optimization pass
    ;if `letrec` defines a non-recursive function, it'll be removed `lecrecc`
    (define (letrec-norec->lambda s)
      (cond
	((not (pair? s)) s)
	((eq? (car s) 'letrec)
	 (let* ((fname (list-ref* s '(1 0 0)))
		(lambda-params (list-ref* s '(1 0 1 1)))
		(lambda-body (list-ref* s '(1 0 1 2)))
		(expr (letrec-norec->lambda lambda-body)))
	   (if (assq fname (count-free-symbols expr))
	       `(letrec ((,fname (lambda ,lambda-params ,expr))) ,fname)
	       `(lambda ,lambda-params ,expr))))
	(else
	  (map letrec-norec->lambda s))))

    ;an optimization pass
    ;delete the arguments that are not be used in `letrecc`
    (define (letrec-del-args s)
      (cond
	((not (pair? s)) s)
	((and (pair? (car s)) (eq? (caar s) 'letrec))
	 (let ((fname (list-ref* s '(0 1 0 0)))
	       (letrec-body (list-ref* s '(0 2)))
	       (lambda-params (list-ref* s '(0 1 0 1 1)))
	       (lambda-body (list-ref* s '(0 1 0 1 2))))
	   (let* ((used (count-free-symbols lambda-body))
		  (remained-params (filter (lambda (x) (assq x used)) lambda-params)))
	     `((letrec ((,fname (lambda ,remained-params ,lambda-body))) ,letrec-body)
	       ,@(map (lambda (x) (list-ref (cdr s) (index lambda-params (lambda (n) (eq? x n))))) remained-params)))))
	(else
	  (map letrec-del-args s))))

    ;make a beta reduce or delete the arguments that are not be used
    (define (beta s)
      (cond
	((not (pair? s)) s)
	((not (pair? (car s))) (map beta s))
	(else
	  (if (eq? (caar s) 'lambda)
	      (let ((lambda-body (list-ref* s '(0 2))))
		(if (and (pair? lambda-body) (eq? (car lambda-body) 'do))
		    `(,(car s) ,@(map beta (cdr s)))
		    (let* ((lambda-params (list-ref* s '(0 1)))
			   (s `((lambda ,lambda-params ,(beta lambda-body)) ,@(map beta (cdr s))))
			   (lambda-body (list-ref* s '(0 2)))
			   (used (count-free-symbols lambda-body))
			   (remained-params (filter (lambda (x) (assq x used)) lambda-params))
			   (beta-params (filter (lambda (x) ((lambda (x) (and x (= 1 (cadr x)))) (assq x used))) lambda-params)))
		      (let ((body (replace-symbols
				     lambda-body
				     (map
				       (lambda (x) (cons x (list-ref (cdr s) (index lambda-params (lambda (n) (eq? x n))))))
				       beta-params))))
			(if (equal? remained-params beta-params)
			    body
			    (let ((params (filter (lambda (x) (not (memq x beta-params))) remained-params)))
			    `((lambda ,params ,body)
			      ,@(map (lambda (x) (list-ref (cdr s) (index lambda-params (lambda (n) (eq? x n))))) params))))))))
	      (map beta s)))))

    ;an optimization pass
    ;for example:
    ;(+ 1 (+ a 2 3) (- b 2) 3) => (+ a 11 (- b))
    (define (combine-add-sub s)
      (define (opposite-op term)
	(cons
	  (case (car term)
	    ((+) '-)
	    ((-) '+))
	  (cdr term)))
      (define (unfold s op)
	(let ((r (map
		   (lambda (x)
		     (if (and (pair? x) (memq (car x) '(+ -)))
			 (unfold (cdr x) (car x))
			 `((+ . ,(combine-add-sub x)))))
		   s)))
	  (case op
	    ((+) (apply append r))
	    ((-) (if (= 1 (length r))
		     (map opposite-op (car r))
		     (append (car r) (map opposite-op (apply append (cdr r)))))))))
      (define (fold s)
	(let ((number
		(fold-left
		  (lambda (r p)
		    (case (car p)
		      ((+) (+ r (cdr p)))
		      ((-) (- r (cdr p)))))
		  0
		  (filter (lambda (x) (number? (cdr x))) s)))
	      (other (filter (lambda (x) (not (number? (cdr x)))) s)))
	  (cond
	    ((null? other) number)
	    (else
	      (let ((other+~ (filter (lambda (x) (eq? '+ (car x))) other))
		    (other-~ (filter (lambda (x) (eq? '- (car x))) other)))
		(let ((other+ (if (> number 0)
				  (cons number (map cdr other+~))
				  (map cdr other+~)))
		      (other- (if (< number 0)
				  (cons (- number) (map cdr other-~))
				  (map cdr other-~))))
		  (cond
		    ((null? other+)
		     (if (= 1 (length other-))
			 `(- ,@(map cdr other-))
			 (if (number? (car other-))
			     `(- ,(- (car other-)) ,@(cdr other-))
			     `(- 0 ,@other-))))
		    ((null? other-)
		     (if (= 1 (length other+))
			 (car other+)
			 `(+ ,@other+)))
		    (else
		      (let ((add_list
			      (if (= 1 (length other+))
				  (car other+)
				  `(+ ,@other+))))
			`(- ,add_list ,@other-))))))))))
      (cond
	((not (pair? s)) s)
	((memq (car s) '(+ -))
	 (fold (unfold (cdr s) (car s))))
	(else (map combine-add-sub s))))

    ;run all the optimization passes
    (let ((passes (list
		    beta
		    letrec-norec->lambda
		    letrec-del-args
		    combine-add-sub
		    )))
      (do
	((r s (fold-left
		(lambda (r pass)
		  ;(printf "~a\n\n" r)
		  (pass r))
		r
		passes))
	 (r-old '() r))
	;if all the passes can not modify the code, the procedure terminates
	((equal? r-old r)
	 ;(printf "LAST:=> ~a\n\n" r)
	 r))))
  (optimize (convert (flag f))))

;call it
(define (call f . s)
 (apply (eval (func->lambda f)) s))
)
