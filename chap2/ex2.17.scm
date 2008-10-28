(define nil (quote ()))

(define (last-pair items)
  (let ((rest (cdr items)))
    (if (null? rest)
        items
        (last-pair rest))))
