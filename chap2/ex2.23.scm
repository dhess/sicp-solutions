(define (for-each f items)
  (if (null? items)
      #t
      (let ()
        (f (car items))
        (for-each f (cdr items)))))
