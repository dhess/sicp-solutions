(define nil (quote ()))

(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (cons (car items) nil))))
