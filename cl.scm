;;; Schönfinkel system: BCIKS
(define (((B f) g) x)
  (f (g x)))
(define (((C f) x) y)
  ((f y) x))
(define (I x) x)
(define ((K x) y) x)
(define (((S f) g) x)
  ((f x) (g x)))

;;; Transform lambda expressions into combinatory
;;;  logic expressions of the BCIKS system.
(define (T expr)
  (cond ((abstraction? expr)
         (let ((e (body expr))
               (x (parameter expr)))
           (cond ((and (and (combination? e)
                            (same? x (operand e)))
                       (not (free? x (operator e))))
                  (T (operator e))) ; eta-reduction
                 ((not (free? x e))
                  (make-combination 'K (T e)))
                 ((same? x e) 'I)
                 ((and (abstraction? e) (free? x (body e)))
                  (T (make-abstraction x (T e))))
                 ((combination? e)
		  (let* ((l (operator e))
			 (r (operand e))
			 (lf (free? x l))
			 (rf (free? x r)))
		    (cond ((and lf rf)
			   (make-combination
			    (make-combination 'S
			     (T (make-abstraction x l)))
			    (T (make-abstraction x r))))
			  (lf
			   (make-combination
			    (make-combination 'C
			     (T (make-abstraction x l)))
			    (T r)))
			  (rf
			   (make-combination
			    (make-combination 'B
			     (T l))
			    (T (make-abstraction x r))))
			  )))
                 (else (error "invalid expression")))))
        ((combination? expr)
         (make-combination (T (operator expr))
                           (T (operand expr))))
        (else expr)))

;;; Print the trace of the transformation
(define (T-trace expr)
  (pp `((T ,expr) =>))
  (cond ((abstraction? expr)
         (let ((e (body expr))
               (x (parameter expr)))
           (cond ((and (and (combination? e)
                            (same? x (operand e)))
                       (not (free? x (operator e))))
                  (pp `((T ,(operator e)) by eta-reduction))
                  (newline)
                  (T-trace (operator e)))
                 ((not (free? x e))
                  (pp `((K (T ,e)) by 3))
                  (newline)
                  (make-combination 'K (T-trace e)))
                 ((same? x e)
                  (pp `(I by 4))
                  (newline)
                  'I)
                 ((and (abstraction? e)
                       (free? x (body e)))
                  (pp `((T ,(make-abstraction x `(T ,e))) by 5))
                  (newline)
                  (T-trace (make-abstraction x (T-trace e))))
                 ((combination? e)
		  (let* ((l (operator e))
			 (r (operand e))
			 (lf (free? x l))
			 (rf (free? x r)))
		    (cond ((and lf rf)
			   (pp `(((S (T ,(make-abstraction x l)))
				  (T ,(make-abstraction x r)))
				 by 6))
			   (newline)
			   (make-combination
			    (make-combination 'S
			     (T-trace (make-abstraction x l)))
			    (T-trace (make-abstraction x r))))
			  (lf
			   (pp `(((C (T ,(make-abstraction x l)))
				  (T ,r))
				 by 7))
			   (newline)
			   (make-combination
			    (make-combination 'C
			     (T-trace (make-abstraction x l)))
			    (T-trace r)))
			  (rf
			   (pp `(((B (T ,l))
				  (T ,(make-abstraction x r)))
				 by 8))
			   (newline)
			   (make-combination
			    (make-combination 'B
			     (T-trace l))
			    (T-trace (make-abstraction x r)))))))
                 (else (error "invalid expression")))))
        ((combination? expr)
         (pp `(((T ,(operator expr)) (T ,(operand expr))) by 2))
         (newline)
         (make-combination (T-trace (operator expr))
                           (T-trace (operand expr))))
        (else
	 (pp `((, expr) by 1))
	 (newline)
	 expr)))

;;; Syntax definitions
(define (abstraction? expr)
  (and (pair? expr) (eq? (car expr) 'lambda)))
(define (make-abstraction var body)
  `(lambda (,var) ,body))
(define (parameter expr) (caadr expr))
(define (body expr) (caddr expr))

(define (combination? expr)
  (and (pair? expr)
       (not (eq? (car expr) 'lambda))))
(define (make-combination operator operand)
  `(,operator ,operand))
(define (operator expr) (car expr))
(define (operand expr) (cadr expr))

(define same? eq?)
(define (free? var expr)
  (cond ((abstraction? expr)
         (if (same? var (parameter expr))
             #f
             (free? var (body expr))))
        ((combination? expr)
         (or (free? var (operator expr))
             (free? var (operand expr))))
        ((same? var expr) #t)
        (else #f)))

