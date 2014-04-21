;; helpers
(define inc
  (lambda (n)
    (+ 1 n)))

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
(define up
  (lambda (lst)
    (cond
     ((null? lst) ())
     ((list? (car lst))
      (append (car lst) (up (cdr lst))))
     (else (cons (car lst) (up (cdr lst)))))))


;; 1.27
(define flatten
  (lambda (lst)
    (if (null? lst) ()
        (let ((elem (car lst)))
          (append
           (cond
            ((list? elem) (flatten elem))
            (else (list elem)))
           (flatten (cdr lst)))))))


;; 1.28
(define merge/predicate
  (lambda (pred lst1 lst2)
    (cond
     ((null? lst1) lst2)
     ((null? lst2) lst1)
     ((pred (car lst1) (car lst2))
      (cons (car lst1)
            (merge/predicate pred (cdr lst1) lst2)))
     (else
      (cons (car lst2)
            (merge/predicate pred lst1 (cdr lst2)))))))

(define merge
  (lambda (lst1 lst2)
    (merge/predicate < lst1 lst2)))


;; 1.29
(define sort
  (lambda (lst)
    (if (null? lst) ()
        (merge (list (car lst))
               (sort (cdr lst))))))


;; 1.30
(define sort/predicate
  (lambda (pred lst)
    (if (null? lst) ()
        (merge/predicate pred
                         (list (car lst))
                         (sort/predicate pred (cdr lst))))))

;; 1.31
(define interior-node
  (lambda (name lson rson)
    (list name lson rson)))

(define interior-node-name
  (lambda (node)
    (car node)))

(define leaf
  (lambda (value)
    value))

(define leaf?
  (lambda (node)
    (integer? node)))

(define lson
  (lambda (node)
    (car (contents-of node))))

(define rson
  (lambda (node)
    (cadr (contents-of node))))

(define contents-of
  (lambda (node)
    (if (leaf? node)
        node
        (cdr node))))

;; 1.32
(define double
  (lambda (num)
    (* 2 num)))

(define double-tree
  (lambda (node)
    (if (leaf? node)
        (leaf (double (contents-of node)))
        (interior-node
         (interior-node-name node)
         (double-tree (lson node))
         (double-tree (rson node))))))


;; 1.33
(define partial-mark-leaves-with-red-depth
  (lambda (num node)
    (if (leaf? node)
        (leaf num)
        (let ((n (if (eqv? 'red (interior-node-name node))
                     (inc num)
                     num)))
          (interior-node
           (interior-node-name node)
           (partial-mark-leaves-with-red-depth n (lson node))
           (partial-mark-leaves-with-red-depth n (rson node)))))))

(define mark-leaves-with-red-depth
  (lambda (node)
    (partial-mark-leaves-with-red-depth 0 node)))

;; test
(mark-leaves-with-red-depth
 (interior-node 'red
  (interior-node 'bar
   (leaf 26)
   (leaf 12))
  (interior-node 'red
   (leaf 11)
   (interior-node 'quux
    (leaf 117)
    (leaf 14)))))


;; 1.34
(define partial-path
  (lambda (plst target lst)
    (cond
     ((null? lst) #f)
     ((= target (interior-node-name lst)) plst)
     (else
      (or
       (partial-path (append plst '(left)) target (lson lst))
       (partial-path (append plst '(right)) target (rson lst)))))))

(define path
  (lambda (target lst)
    (partial-path () target lst)))

;; test
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())
                          ())
                      (31 () ()))))

;; 1.35


(define count-leaf
  (lambda (node)
    (if (leaf? node) 1
        (+ (count-leaf (lson node))
           (count-leaf (rson node))))))

(define partial-number-leaves
  (lambda (num node)
    (if (leaf? node)
        (leaf  num)
    (interior-node
     (interior-node-name node)
     (partial-number-leaves num (lson node))
     (partial-number-leaves (+ num (count-leaf (lson node)))
                            (rson node))))))

(define number-leaves
  (lambda (node)
    (partial-number-leaves 0 node)))

;; test
(number-leaves
    (interior-node 'foo
      (interior-node 'bar
        (leaf 26)
        (leaf 12))
      (interior-node 'baz
        (leaf 11)
        (interior-node 'quux
          (leaf 117)
          (leaf 14)))))


