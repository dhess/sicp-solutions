;; Here's an iterative product procedure:

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; Here's factorial defined in terms of product:

(define (inc n) (+ n 1))

(define (factorial n)
  (define (identity n) n)
  (product identity 1 inc n))

;; And here's an approximation for pi using n terms. See the wiki entry
;; for information on potential problems with this implementation on
;; certain Schemes:
;;
;; http://wiki.drewhess.com/wiki/SICP_exercise_1.31

(define (approximate-pi n)
  (define (numerator n)
    (if (even? n)
        (+ n 2)
        (+ n 3)))
  (define (denominator n)
    (if (even? n)
        (+ n 3)
        (+ n 2)))
  (* (/ (product numerator 0 inc (- n 1))
        (product denominator 0 inc (- n 1)))
     4))

;; Finally, here's a product procedure that generates a recursive
;; process:

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
