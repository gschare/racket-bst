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

(define (contains? tree value)
  (if (null? tree)
      #false
      (let ([x (node-data tree)])
        (cond
          [(= value x) #true]
          [(< value x) (if (null? (node-left tree))
                           #false
                           (contains? (node-left tree) value))]
          [(> value x) (if (null? (node-right tree))
                           #false
                           (contains? (node-right tree) value))]))))

(define (inorder tree)
  (cond
    [(null? tree) ""]
    [else (string-append
           (inorder (node-left tree))
           " "
           (format "~v" (node-data tree))
           " "
           (inorder (node-right tree)))]))

(define (find-min tree)
  (if (null? tree)
      null
      (cond
        [(null? (node-left tree)) (node-data tree)]
        [else (find-min (node-left tree))])))

(define (height tree)
  (if (null? tree)
      -1
      (+ 1 (max (height (node-left tree)) (height (node-right tree))))))

(define root (node 5 null null))
(insert root 3)
(insert root 7)
(insert root 6)


