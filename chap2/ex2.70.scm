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

(define (successive-merge leaf-set)
  (define (successive-merge-iter set result)
    (let ((left (car set))
          (right (cdr set)))
      (if (null? right)
          (make-code-tree left result)
          (successive-merge-iter right (make-code-tree left result)))))
  (successive-merge-iter (cdr leaf-set) (car leaf-set)))

(define (encode message tree) 
  (if (null? message) 
      '() 
      (append (encode-symbol (car message) tree) 
              (encode (cdr message) tree)))) 

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
          '()
          #f)
      (let ((left (encode-symbol symbol
                                 (left-branch tree))))
        (if (list? left)
            (cons 0 left)
            (let ((right (encode-symbol symbol
                                        (right-branch tree))))
              (if (list? right)
                  (cons 1 right)
                  (error "bad symbol -- ENCODE-SYMBOL" symbol)))))))

(define sha-na-na-tree (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (GET 2) (A 2) (JOB 2) (BOOM 1) (WAH 1))))

(length
 (encode
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
  sha-na-na-tree))
