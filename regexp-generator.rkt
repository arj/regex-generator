#lang racket

(provide main)

(require racket/string)
(require data/enumerate)
(require data/enumerate/lib)

;; SETTINGS

;; Standard odering when creating pairs of regular expressions.
(define cons/ordering 'square)
(define no-simplify? #t)
(define sigma-size 5)

;; Definition of standard regular expression
(struct eps ())
(struct empty ())
(struct star (sub))
(struct symbol (c))
(struct union (l r))
(struct concat (l r))

(define regex?
  (or/c star? eps? empty? union? symbol? concat?))

(define (nullable? r)
  (cond
    [(eps? r) #t]
    [(empty? r) #f]
    [(star? r) #t]
    [(symbol? r) #f]
    [(union? r) (or (nullable? (union-l r)) (nullable? (union-r r)))]
    [(concat? r) (and (nullable? (concat-l r)) (nullable? (concat-r r)))]
    ))

;; Definition of omega regular expression
(struct oreg (l r))

;; Smart ctors for symbols

(define (mk-eps)
  (eps))

(define (mk-empty)
  (empty))

(define (mk-symbol c)
  (symbol c))

(define (mk-star s)
  (if no-simplify?
      (star s)
      (cond
        [(star? s) (star (star-sub s))]
        [(eps? s) (mk-eps)]
        [(empty? s) (mk-eps)]
        [else (star s)])))

(define (mk-union l r)
  (if no-simplify?
      (union l r)
      (cond
        [(empty? l) r]
        [(empty? r) l]
        [else (union l r)]
        )))

(define (mk-concat l r)
  (if no-simplify?
      (concat l r)
      (cond
        [(eps? l) r]
        [(or (empty? l) (empty? r)) (mk-empty)]
        [else (concat l r)])))

;; Enumerate regexp symbols using characters
(define resymbol/e
  (map/e
   symbol
   symbol-c
   (take/e char/e sigma-size)
   #:contract symbol?))

;; Enumeration for regular expressions
(define re/e
  (delay/e
   (or/e
    (single/e (eps))
    (single/e (empty))
    resymbol/e
    (map/e
     mk-star
     star-sub
     re/e
     #:contract regex?)
    (map/e
     (λ (p) (mk-union (car p) (cdr p)))
     (λ (r) (cons (union-l r) (union-r r)))
     (cons/e re/e re/e #:ordering cons/ordering)
     #:contract regex?)
    (map/e
     (λ (p) (mk-concat (car p) (cdr p)))
     (λ (r) (cons (concat-l r) (concat-r r)))
     (cons/e re/e re/e #:ordering cons/ordering)
     #:contract regex?)
    )))

(define not-nullable-re/e
  (delay/e
   (or/e
    (single/e (empty))
    resymbol/e
    (map/e
     (λ (p) (mk-concat (car p) (cdr p)))
     (λ (r) (cons (concat-l r) (concat-r r)))
     (cons/e not-nullable-re/e re/e #:ordering cons/ordering)
     #:contract regex?)
    (map/e
     (λ (p) (mk-concat (car p) (cdr p)))
     (λ (r) (cons (concat-l r) (concat-r r)))
     (cons/e re/e not-nullable-re/e #:ordering cons/ordering)
     #:contract regex?)
    (map/e
     (λ (p) (mk-concat (car p) (cdr p)))
     (λ (r) (cons (concat-l r) (concat-r r)))
     (cons/e not-nullable-re/e not-nullable-re/e #:ordering cons/ordering)
     #:contract regex?)
     (map/e
     (λ (p) (mk-union (car p) (cdr p)))
     (λ (r) (cons (union-l r) (union-r r)))
     (cons/e not-nullable-re/e not-nullable-re/e #:ordering cons/ordering)
     #:contract regex?)
     )))

(define one-oreg/e
  (delay/e
   (map/e
    (λ (p) (oreg (car p) (cdr p)))
    (λ (r) (cons (oreg-l r) (oreg-r r)))
    (cons/e re/e not-nullable-re/e #:ordering cons/ordering)
    #:contract oreg?)
   ))

(define oreg/e
  (non-empty-listof/e one-oreg/e))

;; Pretty printer
(define (pp re)
  (cond
    [(empty? re)
     "∅"]
    [(eps? re)
     "ε"]
    [(star? re)
     (string-append (pp (star-sub re)) "*")]
    [(symbol? re)
     (string (symbol-c re))]
    [(union? re)
     (string-append "(" (pp (union-l re)) " + " (pp (union-r re)) ")")]
    [(concat? re)
     (string-append "(" (pp (concat-l re)) "." (pp (concat-r re)) ")")]
    ;; oreg
    [(oreg? re)
     (string-append (pp (oreg-l re)) "." (pp (oreg-r re)) "^ω")]
    [(list? re)
     (string-join (map pp re) " + ")]
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
    ;; oreg
    [(oreg? re)
     (string-append "mkOReg(" (tojava (oreg-l re)) "," (tojava (oreg-r re)) ")")]
    [(list? re)
     (string-join (map tojava re) ",")]
    ))

(define (random-re)
  (from-nat re/e (random-index re/e)))

(define (random-oreg)
  (from-nat oreg/e (random-index oreg/e)))

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
