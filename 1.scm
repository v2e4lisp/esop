;; 1.15
(define duple
  (lambda (n x)
    (if (zero? x) ()
        (cons n (duple n (- x 1))))))


;; 1.16
(define invert
  (lambda (lst)
    (if (null? lst) ()
        (cons (cons (cadar lst) (caar lst))
              (invert (cdr lst))))))


;; 1.17
(define down
  (lambda (lst)
    (if (null? lst) ()
        (cons (cons (car lst) ())
              (down (cdr lst))))))


;; 1.18
(define swapper
  (lambda (s1 s2 lst)
    (if (null? lst) ()
        (let ((elem (car lst)))
          (cons
           (cond
            ((list? elem) (swapper s1 s2 elem))
            ((eqv? elem s1) s2)
            ((eqv? elem s2) s1)
            (else elem))
           (swapper s1 s2 (cdr lst)))))))


;; 1.19
(define list-set
  (lambda (lst n elem)
    (if (null? lst) ()
        (cons
         (if (zero? n) elem (car lst))
         (list-set (cdr lst) (- n 1) elem)))))


;; 1.20
(define partial-count
  (lambda (count s lst)
    (if (null? lst) count
        (let ((elem (car lst)))
            (partial-count (if (list? elem)
                               (partial-count count s elem)
                               (if (eqv? s (car lst)) (+ 1 count) count))
                           s
                           (cdr lst))))))

(define count-occurrences
  (lambda (s lst)
    (partial-count 0 s lst)))


;; 1.21
(define partial-product
  (lambda (sos s)
    (if (null? sos) ()
        (cons (list (car sos) s)
              (partial-product (cdr sos) s)))))

(define product
  (lambda (sos1 sos2)
    (if (null? sos2) ()
        (append (partial-product sos1 (car sos2))
                (product sos1 (cdr sos2))))))


;; 1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst) ()
        (let ((elem (car lst)))
          (if (pred elem)
              (cons elem (filter-in pred (cdr lst)))
              (filter-in pred (cdr lst)))))))

;; 1.23
(define partial-list-index
  (lambda (n pred lst)
    (cond
     ((null? lst) #f)
     ((pred (car lst)) n)
     (else (partial-list-index (+ n 1) pred (cdr lst))))))

(define list-index
  (lambda (pred lst)
    (partial-list-index 0 pred lst)))


;; 1.24
(define every? 
  (lambda (pred lst)
    (if (null? lst) #t
        (and (pred (car lst)) (every? pred (cdr lst))))))

;; 1.25
(define exists? 
  ;; any
  (lambda (pred lst)
    (if (null? lst) #f
        (or (pred (car lst)) (exists? pred (cdr lst))))))

;; 1.26
(define flatten
  (lambda (lst)
    (if (null? lst) ()
        (let ((elem (car lst)))
          (append
           (cond
            ((list? elem) (flatten elem))
            (else (list elem)))
           (flatten (cdr lst)))))))


