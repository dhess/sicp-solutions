(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc k) (+ k 1))

(define (simpsons-rule-integral f a b n)
  (define (helper h)
    (define (y k)
      (f (+ a (* k h))))
    (define (term k)
      (cond ((or (= k 0) (= k n)) (y k))
            ((even? k) (* 2 (y k)))
            (else (* 4 (y k)))))
    (* (sum term 0 inc n)
       (/ h 3)))
  (helper (/ (- b a) n)))
