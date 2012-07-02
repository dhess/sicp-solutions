(define (make-leaf symbol weight) 
  (list 'leaf symbol weight)) 

(define (leaf? object) 
  (eq? (car object) 'leaf)) 

(define (symbol-leaf x) (cadr x)) 

(define (weight-leaf x) (caddr x)) 

(define (make-code-tree left right) 
  (list left 
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right)))) 

(define (left-branch tree) (car tree)) 

(define (right-branch tree) (cadr tree)) 

(define (symbols tree) 
  (if (leaf? tree) 
      (list (symbol-leaf tree)) 
      (caddr tree))) 

(define (weight tree) 
  (if (leaf? tree) 
      (weight-leaf tree) 
      (cadddr tree))) 

(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
        ((< (weight x) (weight (car set))) (cons x set)) 
        (else (cons (car set) 
                    (adjoin-set x (cdr set)))))) 

(define (make-leaf-set pairs) 
  (if (null? pairs) 
      '() 
      (let ((pair (car pairs))) 
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs)))))) 

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; make-leaf-set creates a set ordered from lowest to highest value
(make-leaf-set '((A 4) (B 2) (D 1) (C 1)))

;; but the Huffman tree must be ordered from highest to lowest. See
;; the definition of sample-tree for the general pattern we want
;; successive-merge to follow.

(define (successive-merge leaf-set)
  (define (successive-merge-iter set result)
    (let ((left (car set))
          (right (cdr set)))
      (if (null? right)
          (make-code-tree left result)
          (successive-merge-iter right (make-code-tree left result)))))
  (successive-merge-iter (cdr leaf-set) (car leaf-set)))

(generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
