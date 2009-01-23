;;; In the dialect of Scheme used in the text, the primitive eq? is
;;; only defined when its two arguments are symbols, so in addition to
;;; handling the cases where the arguments to equal? are both lists or
;;; both symbols, we must also handle the cases when one or both are
;;; null?. If we knew that eq? were defined for null? arguments, or
;;; for the case where only one argument is a symbol, we could
;;; simplify this definition.

(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b))
         #f)
        ((and (null? a) (null? b))
         #t)
        ((or (null? a) (null? b))
         #f)
        (else (eq? a b))))
