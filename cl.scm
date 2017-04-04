#| basic usage:
  (T expr) converts lambda to CL
  (L expr) converts CL to lambda
  (P expr) uncurries/prettifies expr into a string
|#

;;; Schönfinkel system: BCIKS
(define (((B f) g) x)
  (f (g x)))
(define (((C f) x) y)
  ((f y) x))
(define (I x) x)
(define ((K x) y) x)
(define (((S f) g) x)
  ((f x) (g x)))


;;;;;;;;;;;;;;;;;;;;
;;; lambda -> CL ;;;
;;;;;;;;;;;;;;;;;;;;

;;; transform lambda expressions into combinatory
;;;  logic expressions of the BCIKS system
(define (T expr)
  (cond
    ((combination? expr)
     (make-combination
      (T (operator expr))
      (T (operand expr))))
    ((abstraction? expr)
     (let ((e (body expr))
           (x (parameter expr)))
       (cond
         ((and (combination? e)
               (same? x (operand e))
               (not (free? x (operator e))))
          (T (operator e))) ; eta-reduction
         ((not (free? x e))
          (make-combination 'K (T e)))
         ((same? x e) 'I)
         ((and (abstraction? e)
               (free? x (body e)))
          (T (make-abstraction x (T e))))
         ((combination? e)
          (let* ((l (operator e))
                 (r (operand e))
                 (lf (free? x l))
                 (rf (free? x r)))
            (cond
              ((and lf rf)
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
              ))))))
    (else expr)))


;;;;;;;;;;;;;;;;;;;;
;;; CL -> lambda ;;;
;;;;;;;;;;;;;;;;;;;;

;;; transform combinatory logic expressions into lambda expressions
(define (L expr)
  (reify (beta-reduce (L-index expr))))

(define (reify expr)
  ; because we're coming from CL, there are no free de bruijn indices.
  ; reify creates a concrete variable name on every use of abstraction,
  ;  and uses de bruijn index substitution to update its occurences.
  (define next 0)
  (let r ((e expr))
    (cond ((indexed-abstraction? e)
           (set! next (+ next 1))
           (let* ((name (reify-name next))
                  (t (indexed-body e))
                  (ne (substitute t 0 name)))
             (make-abstraction name (r ne))))
          ((combination? e)
           (make-combination
            (r (operator e))
            (r (operand e))))
          (else e))))
(define (beta-reduce expr)
  ; repeats beta-reduction until no change
  (let repeat ((br (beta-reduce-inner expr)))
    (let ((did-reduce (cdr br))
          (expr (car br)))
      (if did-reduce
          (repeat (beta-reduce-inner expr))
          expr))))
(define (beta-reduce-inner expr)
  ; does beta-reduction.
  ; returns pair: (expr . did-reduce)
  (cond ((and (combination? expr)
              (indexed-abstraction? (operator expr)))
         (let* ((t (indexed-body (operator expr)))
                (v (operand expr))
                (br (beta-reduce-inner
                     (shift 0 -1 (substitute t 0 (shift 0 1 v))))))
           (cons (car br) #t)))
        ((indexed-abstraction? expr)
         (let* ((br (beta-reduce-inner (indexed-body expr)))
                (e (make-indexed-abstraction (car br))))
           (cons e (cdr br))))
        ((combination? expr)
         (let* ((brl (beta-reduce-inner (operator expr)))
                (brr (beta-reduce-inner (operand expr)))
                (e (make-combination (car brl) (car brr))))
           (cons e (or (cdr brl) (cdr brr)))))
        (else (cons expr #f))))
(define (substitute expr j s)
  ; [j->s](expr) substitutes index j with term s in expr.
  (cond ((index? expr)
         (let ((k (get-index expr)))
           (if (= k j) s (make-index k))))
        ((indexed-abstraction? expr)
         (let ((t (indexed-body expr)))
           (make-indexed-abstraction
            (substitute t (+ j 1) (shift 0 1 s)))))
        ((combination? expr)
         (make-combination
          (substitute (operator expr) j s)
          (substitute (operand expr) j s)))
        (else expr)))
(define (shift c d expr)
  ; \uparrow_c^d(expr) shifts indices above cutoff c in expr by amount d.
  (cond ((index? expr)
         (let ((k (get-index expr)))
           (make-index (if (< k c) k (+ k d)))))
        ((indexed-abstraction? expr)
         (let ((t (indexed-body expr)))
           (make-indexed-abstraction
            (shift (+ c 1) d t))))
        ((combination? expr)
         (make-combination
          (shift c d (operator expr))
          (shift c d (operand expr))))
        (else expr)))
(define (L-index expr)
  ; convert CL to de bruijn indexed lambda calculus.
  (cond ((same? 'I expr)
         (make-indexed-abstraction (make-index 0)))
        ((same? 'K expr)
         (make-indexed-abstraction
          (make-indexed-abstraction (make-index 1))))
        ((same? 'C expr)
         (make-indexed-abstraction
          (make-indexed-abstraction
           (make-indexed-abstraction
            (list->indexed '((2 0) 1))))))
        ((same? 'B expr)
         (make-indexed-abstraction
          (make-indexed-abstraction
           (make-indexed-abstraction
            (list->indexed '(2 (1 0)))))))
        ((same? 'S expr)
         (make-indexed-abstraction
          (make-indexed-abstraction
           (make-indexed-abstraction
            (list->indexed '((2 0) (1 0)))))))
        ((combination? expr)
         (make-combination
          (L-index (operator expr))
          (L-index (operand expr))))
        (else expr)))


;;;;;;;;;;;;;;
;;; pretty ;;;
;;;;;;;;;;;;;;

;;; turns an expression into an uncurried string
(define (P e)
  (P-join (P-lift-singletons (P-stringify (P-uncurry e)))))

(define (P-uncurry e)
  ; uncurry and turn abstractions into strings
  (let rec ((e e))
    (cond
      ((combination? e)
         (append (rec (operator e))
                 ((if (combination? (operand e))
                      list I) ; wrap combination in list
                  (rec (operand e)))))
      ((abstraction? e)
       (let* ((x (parameter e))
              (b (body e))
              (nb (rec b)))
         (if (abstraction? b)
             (P-make-abstraction-multi x nb) ; λxy instead of λx λy
             (P-make-abstraction x nb))))
      (else (list e)))))
(define (P-stringify e)
  ; turn every node into a string
  (let r ((e e))
    (cond
      ((list? e) (map r e))
      ((P-abstraction? e)
       (string-append e "."))
      ((string? e) e)
      ((symbol? e) (symbol->string e))
      (else e))))
(define (P-lift-singletons e)
  ; lift singletons
  (let r ((e e))
    (cond
      ((and (list? e) (= 1 (length e))) (car e))
      ((list? e) (map r e))
      (else e))))
(define (P-join e)
  ; join nodes into a string
  (define (join r e)
    (fold-left
      (lambda (a b)
        (string-append a " " b))
      (r (car e))
      (map r (cdr e))))
  (define (r e)
    (cond
      ((and (list? e)
            (P-abstraction? (car e))) ; λa. b -> λa.b
       (string-append "(" (car e) (join r (cdr e)) ")"))
      ((list? e) (string-append "(" (join r e) ")"))
      (else e)))
  (let ((joined (r e)))
    (if (char=? #\( (string-ref joined 0))
        ; remove outer parentheses
        (substring joined 1 (- (string-length joined) 1))
        joined)))

; λ-string abstractions
(define (P-abstraction? e)
  (and (string? e)
       (> (string-length e) 1)
       (string=? "λ" (substring e 0 2))))
(define (P-make-abstraction-multi x nb)
  (list
    (cons
      (string-append "λ" (symbol->string x)
                     (string-tail (caar nb) 1))
      (cdar nb))))
(define (P-make-abstraction x nb)
  (list
    (cons
      (string-append "λ" (symbol->string x))
      nb)))


;;;;;;;;;;;;;;;;;;;;
;;;    Syntax    ;;;
;;;;;;;;;;;;;;;;;;;;

(define *index* (string->uninterned-symbol "index"))
(define *lambda-indexed* (string->uninterned-symbol "lambda-indexed"))

; classic lambda expressions
(define (abstraction? expr)
  (and (pair? expr) (eq? (car expr) 'lambda)))
(define (make-abstraction var body)
  `(lambda (,var) ,body))
(define (parameter expr) (caadr expr))
(define (body expr) (caddr expr))

; curried function application
(define (combination? expr)
  (and (pair? expr)
       (not (eq? (car expr) 'lambda))
       (not (eq? (car expr) *index*))
       (not (eq? (car expr) *lambda-indexed*))))
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


; de bruijn index
(define (index? idx)
  (and (pair? idx) (eq? (car idx) *index*)))
(define (make-index n) `(,*index* ,n))
(define (get-index expr) (cadr expr))

; variable-free (indexed) lambda abstraction
(define (indexed-abstraction? expr)
  (and (pair? expr) (eq? (car expr) *lambda-indexed*)))
(define (make-indexed-abstraction body)
  `(,*lambda-indexed* ,body))
(define (indexed-body expr) (cadr expr))
(define (list->indexed lst)
  (let walk ((l lst))
    (cond ((pair? l)
           (cons (walk (car l)) (walk (cdr l))))
          ((number? l)
           (make-index l))
          (else l))))

; reify to go from indices to concrete variable names
(define reify-alphabet "abcdefghijklmnopqrstuvwxyz")
(define (reify-name n)
  (let* ((total (string-length reify-alphabet))
         (n (- n 1))
         (m (quotient n total))
         (c (remainder n total))
         (l (string-ref reify-alphabet c)))
    (string->symbol (make-string (+ m 1) l))))

