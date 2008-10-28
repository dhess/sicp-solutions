(define (fast-mul a b)
  (fast-mul-helper a b 0))

(define (fast-mul-helper a b product)
  (cond
   ((= b 0) product)
   ((even? b) (fast-mul-helper (double a) (halve b) product))
   (else (fast-mul-helper a (- b 1) (+ product a)))))
