(define (double a)
  (* a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (halve a)
  (/ a 2))

(define (fast-mul a b)
  (cond
   ((= b 0) 0)
   ((even? b) (double (fast-mul a (halve b))))
   (else (+ a (fast-mul a (- b 1))))))
