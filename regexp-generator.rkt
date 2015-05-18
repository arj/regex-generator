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

;; Standard odering when creating pairs of regular expressions.
(define cons/ordering
  'diagonal)

;; Enumeration for regular expressions
(define re/e
  (delay/e
   (or/e
    (single/e (eps))
    (single/e (empty))
    (map/e
     symbol
     symbol-c
     (take/e char/e 3)
     #:contract symbol?)
    (map/e
     star
     star-sub
     re/e
     #:contract star?)
    (pam/e
     (λ (p) (union (car p) (cdr p)))
     (cons/e re/e re/e #:ordering cons/ordering)
     #:contract union?)
    (pam/e
     (λ (p) (concat (car p) (cdr p)))
     (cons/e re/e re/e #:ordering cons/ordering)
     #:contract concat?)
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

(define (main . xs)
  (for-each
   (λ (x)
     (begin
       (display (pp x))
       (display ";")
       (newline)))
   (enum->list re/e (string->number (car xs)))))