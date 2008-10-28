; Filtered accumulate:

(define (filtered-accumulate combiner null-value predicate term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate a)
                  (combiner (term a) result)
                  result))))
  (iter a null-value))

; 1.33a sum of the squares of the prime numbers in the interval [a,b],
; using prime? from exercise 1.23:

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (inc n) (+ n 1))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

; 1.33b product of all positive integers less than n that are
; relatively prime to n, using the iterative gcd procedure from
; exercise 1.20:

(define (identity n) n)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-relative-primes n)
  (define (relatively-prime-to-n? a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 relatively-prime-to-n? identity 2 inc (- n 1)))
