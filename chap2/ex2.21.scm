(define (square x) (* x x))

(define nil (quote ()))

;; First definition:

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

;; Second definition:

(define (square-list items)
  (map square items))
