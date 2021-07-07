;;Name: Parry, Craig
;;Net ID:  parryc@unm.edu
;; cs357hw3

;; === Part 1, Points: 10, Weight: 1/3 ===
;; x, k(x) = f(g(h(x)))



(define compose
    (lambda (f g)
        (lambda (x)
            (f (g x))
        )
    )
)

;; problem1-1-7.2    p234
;; Points: 1/10

;;((compose3 add1 sqrt add1) 8) => 4
(define compose3
  (lambda (f g h)
    (lambda (x)
      ((compose f (compose g h))x))))

;; problem1-1-7.3    p234
;; Points: 1/10
;;((compose-many add1 add1 add1 add1) 3) => 7
;;((compose-many sqrt abs sub1 (lambda (n) (* n n))) 0.6) => 0.8
;;(let ((f (lambda (n) (if (even? n) (/ n 2) (add1 n)))))
;;  ((compose-many f f f f f f ) 21)) => 4

(define compose-many 
  (lambda args
    (lambda (x)
      (letrec 
	  ((loop 
	    (lambda (args)
	      (if (null? args)
		  x
		  ((car args) (loop (cdr args)))))))
	(loop args)))))   

;; problem1-1-7.6    p235
;; Points: 1/10

;;(map-first-two + '(2 3 4 5 7)) => (5 7 9 12)
;;(map-first-two max '(2 4 3 5 4 1)) => (4 4 5 5 4)

(define map-first-two
  (lambda (proc ls) 
    (if (null? (cdr ls)) 
	'()
	(cons (proc (car ls) (cadr ls)) (map-first-two proc (cdr ls))))))

;; problem1-1-7.7    p235
;; Points: 1/10

;;(reduce + '(1 3 5 7 9)) => 25
;;(reduce max '(2 -4 6 8 3 1)) => 8
;;(reduce (lambda (x y) (and x y)) '(#t #t #t #t)) => #t

(define reduce 
  (lambda (proc ls)
    (if (null? (cddr ls))
	(proc (car ls) (cadr ls))
	(reduce proc (cons (proc (car ls) (cadr ls)) (cddr ls))))))

;; problem1-1-7.8    p236
;; Points: 1/10

;;(andmap positive? '(3 4 6 9)) => #t
;;(andmap positive? '(3 -1 4 8)) => #f
;;(let ((not-null? (compose not null?)))
;;  (andmap not-null? '((a b) (c) (c d e)))) => #t

(define andmap
  (lambda (pred ls)
    (if (null? (cdr ls)) 
	(pred (car ls))
	(if (and (pred (car ls)) #t)
	    (andmap pred (cdr ls))
	    (pred (car ls))))))

;; problem1-1-7.12    p243
;; Points: 1/10

;;((curried* 25) 5) => 125
;;(times10 125) => 1250

(define curried* 
  (lambda (x) 
    (lambda (n)
      (* x n))))

(define times10 (curried* 10))

;; problem1-1-7.18    p244
;; Points: 1/10

;;(((between?-c 5)6)7) => #t
;;(((between?-c 5)5)7) => #f
;;(((between?-c 5)4)7) => #f

(define between? 
  (lambda (x y z) (and (> y x) (< y z))))

(define between?-c
  (lambda (x)
    (lambda (y)
      (lambda (z) 
	(and (> y x) (< y z))))))

;; problem1-1-7.22    p250
;; Points: 1/10

;;((mult-by-scalar 3) '(1 -2 3 -4)) => (3 -6 9 -12)
;;((mult-by-scalar 5) '()) => ()

(define mult-by-scalar
  (lambda (c)
    (lambda (ntpl)
      ((flat-recur '() (lambda (x y) (cons (* c x) y))) ntpl))))

(define flat-recur
  (lambda (seed list-proc)
    (letrec 
	((helper
	  (lambda (ls) 
	    (if (null? ls)
		seed
		(list-proc (car ls) (helper (cdr ls)))))))
      helper)))


;; problem1-1-7.30    p
;; Points: 1/10

(define reverse-all
  (lambda (ls)
    ((deep-recur '()
		 (lambda (x y) (append y (list x)))
		 (lambda (x y) (if (null? x) 
				   (append y x)
				   (if (null? y) 
				      (list x)
				       (append y (list x))))))
     ls)))
;;defined deep-recur for the fucntion to use
(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
	((helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(let ((a (car ls)))
		  (if (or (pair? a) (null? a))
		      (list-proc (helper a) (helper (cdr ls)))
		      (item-proc a (helper (cdr ls)))))))))
      helper)))

;; problem1-1-7.31    p
;; Points: 1/10

;; ********* Lance said we can skip this problem *******
;;only works for list that do not have nested pairs

(define flat-recur
  (lambda (seed list-proc)
    (lambda (ls) 
      (letrec 
	  ((item-proc (lambda (x y) (list-proc x y))))
	((deep-recur seed list-proc item-proc) ls)))))
	    

;; === Part 2, Points: 10, Weight: 1/3 ===

;; problem2-1-a
;; Points: 1/8

;;(define fact (tail-recur zero? sub1 * 1))
;;(fact 10) => 3628800

(define tail-recur 
  (lambda (bpred xproc aproc acc0)
    (lambda (x)
      (letrec 
	  ((loop 
	    (lambda (x acc0) 
	      (if (bpred x) 
		  acc0 
		  (loop (xproc x) (aproc x acc0))))))
	(loop x acc0)))))


(define fact (tail-recur zero? sub1 * 1))  

;; problem2-1-b
;; Points: 1/8

(define reverse (tail-recur null? cdr (lambda (x y) (cons (car x) y)) '()))

;; problem2-1-c
;; Points: 1/8

(define iota (tail-recur zero? sub1 cons '()))

;; problem2-2-
;; Points: 1/8

;;((disjunction2 symbol? procedure?) +) => #t
;;((disjunction2 symbol? procedure?) (quote +)) => #t
;;(filter (disjunction2 even? (lambda (x) (< x 4))) (iota 8)) => (1 2 3 4 6 8)

(define disjunction2 
  (lambda (sym proc)
    (lambda (x)
      (or (sym x) (proc x)))))

;; problem2-3-
;; Points: 1/8

(define disjunction
  (lambda args
    (lambda (x)
      (letrec
          ((loop
            (lambda (args)
              (if (null? (cdr args))
                ((car args) x)
                 (or ((car args) x) (loop (cdr args)))))))
        (loop args)))))

;; problem2-4-
;; Points: 1/8

;;(matrix-map (lambda (x) (* x x)) '((1 2) (3 4))) => ((1 4) (9 16))

(define matrix-map
  (lambda (fx lls) 
    (map (lambda (x) (map fx (car (list x))))  lls)))

;; problem2-5

(define fold 
  (lambda (seed proc) 
    (letrec 
	((pattern 
	  (lambda (ls) 
	    (if (null? ls) 
		seed 
		(proc (car ls) 
		      (pattern (cdr ls)))))))
      pattern)))

;; problem2-5-a
;; Points: 1/8
;;(delete-duplicates '(a b a b a b a b)) = > (a b) 
;;(delete-duplicates '(1 2 3 4)) => (1 2 3 4) 

(define delete-duplicates 
  (lambda (ls) 
    (letrec 
	((delete
	  (lambda (x y)
	    (cond 
	     ((null? y) (list x))
	     ((equal? x (car y)) '())
	     (else (delete x (cdr y)))))))
      ((fold '() (lambda (x y)
		  (append (delete x y) y)))ls))))

;; problem2-5-b
;; Points: 1/8

;;(assoc 'b '((a 1) (b 2))) => (b 2)
;;(assoc 'c '((a 1) (b 2))) => #f

(define assoc
  (lambda (item lls) 
    (car ((fold '() (lambda (x y)
		      (if (equal? item (car x))
			  (cons x y) 
			  (if (null? y)
			      (list #f)
			      (append '() y)))))lls))))

;; === Part 3, Points: 8, Weight: 1/3 ===

;; problem3-1-
;; Points: 1/8

;;length -returns the length of a list

(define length
  (lambda (ls)
    (apply + (map (lambda (x) 1) ls))))
 
;; problem3-2-
;; Points: 1/8

;;sum-of-squares -returns the sum of the sqares of its arguments
;;(sum-of-squares 1 2 3 4 5) => 55

(define sum-of-squares
  (lambda args
    (apply + (map (lambda (x) (* x x)) args))))

;; problem3-3-
;; Points: 1/8

;;avg -returns the average of its arguments
;;(avg 1 2 3 4 5 6 7 8) => 9/2

(define avg
  (lambda args 
    (/ (apply + args) (apply + (map (lambda (x) 1) args)))))

;; problem3-4-
;; Points: 1/8

;;avg-odd -returns the average of its odd arguments
;;assumes avg-odd is (sum-of-odds/#-of-odds)
;; and numberically odd not sequentially odd

;;(avg-odd 1 2 3 4 5 6 7) => 4

(define avg-odd
  (lambda args 
    (let ((average
	  (lambda (ls)
	    (/ (apply + ls) (apply + (map (lambda (x) 1) ls))))))
     (average (filter odd? args)))))

;; problem3-5-
;; Points: 1/8

;;shortest -returns the shortest of its list arguments
;;(shortest '(1 2 3 4) '(1 2) '(1 2 3) '(1) '(1 2 3 4 5)) => '((1))


(define shortest 
  (lambda args
    (let*
        ((length
          (lambda (list) (if (pair? list) 
            (apply + (map (lambda (x) 1) list)) 10000000000)))
	 (not-null?
	  (lambda (x)
	    (not (null? x)))))
      (filter not-null? (map (lambda (x) 
			       (if (eq? (apply min  (map length args)) (length x))
				   x 
				   '()))  args)))))
  
;; problem3-6-
;; Points: 1/8

;;avg-fact -returns the average of the factorials of its arguments
;;(avg-fact 1 2 3 4) => 33/4

(define avg-fact
  (lambda args 
    (let
	((fact (lambda (x)
	       (apply * (iota x))))
	 (average
          (lambda (ls)
            (/ (apply + ls) (apply + (map (lambda (x) 1) ls))))))
      (average (map fact args)))))

;; problem3-7-
;; Points: 1/8

;;tally -takes a predicate and a list and returns the number
;; of list elements which satisfy the predicate
;;(tally null? '(1 2 3 () 4 5)) => 1

(define tally 
  (lambda (pred ls) 
    (apply + (map (lambda (x) (if (pred x) 1 0)) ls))))
 

;; problem3-8-
;; Points: 1/8

;;list-ref -takes a list and an interger,n, and returns the n-th element of the list
;;assumes elements are ranged 1...n

;;(list-ref '(5 4 3 2 1) 3) => 3
;;(list-ref '(1 5 9 85 7 2 1) 4) => 85

(define list-ref
  (lambda (list n)
    (let
	((length 
	  (lambda (ls)
	    (apply + (map (lambda (x) 1) ls)))))
      (cdar (filter (lambda (x) (equal? n (car x))) (map cons (iota (length list)) list)
)))))

