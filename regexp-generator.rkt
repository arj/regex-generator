#lang racket

(provide main)

(require data/enumerate)
(require data/enumerate/lib)

;; Definition of standard regular expression
(struct eps ())
(struct empty ())
(struct star (sub))
(struct symbol (c))
(struct union (l r))
(struct concat (l r))

(define regex?
  (or/c star? eps? empty? union? symbol? concat?))

;; Standard odering when creating pairs of regular expressions.
(define cons/ordering
  'square)

;; Smart ctors for symbols

(define (mk-union l r)
  (cond
    [(empty? l) r]
    [(empty? r) l]
    [else (union l r)]
    ))

(define (mk-eps)
  (eps))

(define (mk-empty)
  (empty))

(define (mk-star s)
  (cond
    [(star? s) (star (star-sub s))]
    [(eps? s) (mk-eps)]
    [(empty? s) (mk-empty)]
    [else (star s)]))

(define (mk-symbol c)
  (symbol c))

(define (mk-concat l r)
  (cond
    [(eps? l) r]
    [(or (empty? l) (empty? r)) (mk-empty)]
    [else (concat l r)]))

;; Enumeration for regular expressions
(define re/e
  (delay/e
   (or/e
    (single/e (eps))
    (single/e (empty))
    (pam/e
     symbol
     (take/e char/e 5)
     #:contract symbol?)
    (pam/e
     mk-star
     re/e
     #:contract regex?)
    (pam/e
     (λ (p) (mk-union (car p) (cdr p)))
     (cons/e re/e re/e #:ordering cons/ordering)
     #:contract regex?)
    (pam/e
     (λ (p) (mk-concat (car p) (cdr p)))
     (cons/e re/e re/e #:ordering cons/ordering)
     #:contract regex?)
    )
   #:two-way-enum? #f
   ))

;; Pretty printer
(define (pp re)
  (cond
    [(empty? re)
     "{}"]
    [(eps? re)
     "#"]
    [(star? re)
     (string-append (pp (star-sub re)) "*")]
    [(symbol? re)
     (string (symbol-c re))]
    [(union? re)
     (string-append "(" (pp (union-l re)) " + " (pp (union-r re)) ")")]
    [(concat? re)
     (string-append "(" (pp (concat-l re)) "." (pp (concat-r re)) ")")]
    ))

;; To java expression
(define (tojava re)
  (cond
    [(empty? re)
     "mkEmpty()"]
    [(eps? re)
     "mkEps()"]
    [(star? re)
     (string-append "mkStar(" (tojava (star-sub re)) ")")]
    [(symbol? re)
     (string-append "mkSymbol('" (string (symbol-c re)) "')")]
    [(union? re)
     (string-append "mkUnion(" (tojava (union-l re)) "," (tojava (union-r re)) ")")]
    [(concat? re)
     (string-append "mkConcat(" (tojava (concat-l re)) "," (tojava (concat-r re)) ")")]
    ))

(define (random-re)
  (let ([x (- (random 4294967087) 1)])
    (from-nat re/e x)))

(define (gen-random-re n)
  (if (= n 0)
      null
      (cons (random-re) (gen-random-re (- n 1)))))

(define (main . xs)
  (for-each
   (λ (x)
     (begin
       (display (pp x))
       (newline)))
   (enum->list re/e (string->number (car xs)))))
