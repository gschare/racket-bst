#lang racket

(struct node (data left right) #:transparent #:mutable)
; a binary tree (BT) is one of the following:
;   - null
;   - (node Number BT BT)

; a binary search tree (BST) is a BT with the property that the data of the node to the left and
; to the right are less than and greater than the current node's data, respectively

; insert : BT Number -> BT
; updates tree with the insertion of value at appropriate location and returns it
(define (insert tree value)
  (if (null? tree)
      (node value null null)
      (let ([x (node-data tree)])
        (cond
          [(= value x) tree]
          [(< value x) (if (null? (node-left tree))
                           (set-node-left! tree (node value null null))
                           (insert (node-left tree) value))]
          [(> value x) (if (null? (node-right tree))
                           (set-node-right! tree (node value null null))
                           (insert (node-right tree) value))])
        tree)))

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
      (let ([x (node-data tree)])
        (cond
          [(= value x) #true]
          [(< value x) (contains? (node-left tree) value)]
          [(> value x) (contains? (node-right tree) value)]))))

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
  (if (null? tree)
      -1
      (+ 1 (max (height (node-left tree)) (height (node-right tree))))))

; driver
(define n 100) ; max number for random values
; generate a tree of a random root and 10 random numbers between 0 and n
(define mytree (insert-list (node (random n) null null) (map (λ (_) (random n)) (range 0 10 1))))
; print the tree as-is
mytree
; print the tree inorder
(inorder mytree)


