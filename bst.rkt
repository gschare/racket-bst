#lang racket

(struct node (data left right) #:transparent #:mutable)
; a binary tree (BT) is one of the following:
;   - null
;   - (node Number BT BT)

; a binary search tree (BST) is a BT with the property that the data of the node to the left and
; to the right are less than and greater than the current node's data, respectively

; maximum difference in heights allowed in each subtree
(define MAX-IMBALANCE 1)

; balance : BT -> BT
; balances the tree using AVL
(define (balance tree)
  (cond
    [(null? tree) tree]
    [(> (- (height (node-left tree)) (height (node-right tree))) MAX-IMBALANCE)
     (cond
       [(>= (height (node-left (node-left tree))) (height (node-right (node-left tree))))
        (rotate-with-left tree)]
       [else (double-rotate-with-left tree)])]
    [(> (- (height (node-right tree)) (height (node-left tree))) MAX-IMBALANCE)
     (cond
       [(>= (height (node-right (node-right tree))) (height (node-left (node-right tree))))
        (rotate-with-right tree)]
       [else (double-rotate-with-right tree)])]
    [else tree]))

; rotate-with-left : BT -> BT
(define (rotate-with-left tree)
   (node (node-data (node-left tree))
          (node-left (node-left tree))
          (node (node-data tree) (node-right (node-left tree)) (node-right tree))))

; rotate-with-right : BT -> BT
(define (rotate-with-right tree)
  (node (node-data (node-right tree))
        (node (node-data tree) (node-left tree) (node-left (node-right tree)))
        (node-right (node-right tree))))

; double-rotate-with-left : BT -> BT
(define (double-rotate-with-left tree)
  (set-node-left! tree (rotate-with-right (node-left tree)))
  (rotate-with-left tree))

; double-rotate-with-right : BT -> BT
(define (double-rotate-with-right tree)
  (set-node-right! tree (rotate-with-left (node-right tree)))
  (rotate-with-right tree))

; insert : BT Number -> BT
; updates tree with the insertion of value at appropriate location and returns it
(define (insert tree value)
  (if (null? tree)
      (node value null null)
      (let ([current (node-data tree)])
        (cond
          [(= value current) tree]
          [(< value current) (if (null? (node-left tree))
                           (set-node-left! tree (node value null null))
                           (insert (node-left tree) value))]
          [(> value current) (if (null? (node-right tree))
                           (set-node-right! tree (node value null null))
                           (insert (node-right tree) value))])
        (balance tree))))

; insert-list : BT List-of-Numbers -> BT
; repeatedly insert a list of numbers into the tree in the order they're provided
(define (insert-list tree alon)
  (cond
    [(null? alon) tree]
    [else (insert-list (insert tree (first alon)) (rest alon))]))

; contains : BT Number -> Boolean
; checks if the tree contains a particular value
(define (contains? tree value)
  (if (null? tree)
      #false
      (let ([current (node-data tree)])
        (cond
          [(= value current) #true]
          [(< value current) (contains? (node-left tree) value)]
          [(> value current) (contains? (node-right tree) value)]))))

; inorder : BT -> List-of-Numbers
; return every value of the tree in value-sorted order (inorder traversal)
(define (inorder tree)
  (cond
    [(null? tree) empty]
    [else (append
           (inorder (node-left tree))
           (list (node-data tree))
           (inorder (node-right tree)))]))

; find-min : BT -> Number
; return the smallest value in the tree
(define (find-min tree)
  (if (null? tree)
      null
      (cond
        [(null? (node-left tree)) (node-data tree)]
        [else (find-min (node-left tree))])))

; find-max : BT -> Number
; return the largest value in the tree
(define (find-max tree)
  (if (null? tree)
      null
      (cond
        [(null? (node-right tree)) (node-data tree)]
        [else (find-max (node-right tree))])))

; height : BT -> Number
; return the height of the tree
(define (height tree)
  (cond
    [(null? tree) -1]
    [else (+ 1 (max (height (node-left tree)) (height (node-right tree))))]))

; height-tr : BT -> Number
; return the height of the tree
; uses tail recursion
(define (height-tr tree)
  (define (recurse tree depth)
    (cond
    [(null? tree) depth]
    [else (max
           (recurse (node-left tree) (add1 depth))
           (recurse (node-right tree) (add1 depth)))]))
  (recurse tree -1))

; tester

; generate a tree of n random numbers between 0 and 100n (for uniqueness w.h.p.)
(define n 20) ; how many random numbers to create
(define rand-list (build-list n (λ (_) (random (* 100 n)))))
(define mytree (insert-list null rand-list))

mytree ; print the tree as-is
(inorder mytree) ; print the tree inorder

(define (unique list)
  (define (recurse list list-so-far)
    (cond
      [(null? list) list-so-far]
      [(not (member (first list) list-so-far))
       (recurse (rest list) (cons (first list) list-so-far))]
      [else
       (recurse (rest list) list-so-far)]))
  (recurse list empty))

(define (unique? list)
  (= (length list) (length (unique list))))

(unique? rand-list) ; display whether or not the random numbers generated were all unique
                    ; this should be true with high probability
