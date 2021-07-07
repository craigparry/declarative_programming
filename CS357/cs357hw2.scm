;;Name: Parry, Craig
;; Net ID:  parryc@unm.edu 
;; cs357hw2

;;Problem 1-4.4 p101

;;deepen-1 wraps a pair of parenthesis around each top level item of the list
;;i/o
;; (deepen-1 '(a b c d)) =>((a) (b) (c) (d))
;; (deepen-1 '((a b) (c (d e)) f)) =>(((a b)) ((c (d e))) (f)) 
;; (deepen-1 '()) =>() 

(define deepen-1
  (lambda (ls)
    (cond 
     ((null? ls) '())
     (else (cons (cons (car ls) '()) (deepen-1 (cdr ls)))))))

;;problem1-4.6 p108

;;(insert-left-all 'z 'a '(a ((b a) ((a (c)))))) => (z a ((b z a) ((z a (c)))))
;;(insert-left-all 'z 'a '(((a)))) => (((z a)))
;;(insert-left-all 'z 'a '()) => ()

(define insert-left-all
  (lambda (new old ls)
    (cond 
     ((null? ls) '())
     ((eq? (car ls) old) 
      (cons new (cons (car ls) (insert-left-all new old (cdr ls)))))
     ((pair? (car ls))
      (cons (insert-left-all new old (car ls))
	    (insert-left-all new old (cdr ls))))
     (else (cons (car ls) (insert-left-all new old (cdr ls)))))))

;;problem1-4.10  p115

;;leftmost that takes a nonempty list as its argument and
;;returns the leftmost atomic item in the list.

;;(leftmost '((a b) (c (d e)))) => a
;;(leftmost '((((c ((e f) g) h))))) => c
;;(leftmost '(() a)) => ()

(define leftmost
  (lambda (ls)
    (cond 
     ((null? ls) '())
     ((not (pair? (car ls))) (car ls))
     (else (leftmost (car ls))))))


;;problem 1-4.11 p115

;;rightmost that takes a nonempty list as its argument and
;;returns the rightmost atomic item in the list

;;(rightmost '((a b) (d (c d (f (g h) i) m n) u) v)) => v 
;;(rightmost '((((((b (c)))))))) => c
;; (rightmost '(a ())) => ()

(define rightmost
  (lambda (ls)
    (letrec 
	((flatten
	  (lambda (ls)
	    (cond
	     ((null? ls) '())
	     ((pair? (car ls))
	      (append (flatten (car ls)) (flatten (cdr ls))))
	     (else (cons (car ls) (flatten (cdr ls))))))))
      (leftmost (reverse (flatten ls))))))

;;problem1-4.18 p128

;;Write an iterative version length-it of the procedure length, that computes
;;the length of a list.

;;(length-it '(a b c d e)) => 5
;;(length-it '(1 (2 3) (4 5 6))) => 3
;;(length-it '(one)) => 1
;;(length-it '()) => 0

(define length-it
  (lambda (ls)
    (letrec 
	((len-help
	  (lambda (ls acc)
	    (if (null? ls)
		acc
		(add1 (len-help (cdr ls) acc))))))
      (len-help ls 0))))
;;problem 1-4.19 p128

;; an iterative procedure mk-asc-list-of-ints that,
;; for any integer n, produces a list of the integers from 1 to n
;; in ascending order.
(define mk-asc-list-of-ints
  (lambda (n) 
    (letrec
	((ascending-list
	   (lambda (n ls)
	     (if (= n 0)
		 ls
		 (ascending-list (sub1 n) (cons n ls))))))
      (ascending-list n '()))))
     
;; An iterative procedure mk-desc-list-of-ints that,
;; for any integer n, produces a list of integers from
;; n to 1 in descending order.

(define mk-desc-list-of-ints
  (lambda (n)
    (letrec
	((descending-list
	  (lambda (n ls)
	    (if (= n 0)
		ls
		(descending-list (sub1 n) (append ls (list n)))))))
      (descending-list n '()))))

;;problem 1-4.20 p128

;;Define both recursive and iterative versions of a procedure
;;occurs that counts the number of times an item occurs at the 
;;top level in a list. Call the iterative version occurs-it.
;;Test your procedures by counting how many times the item a
;; occurs at top level in each of the following lists:

;;(a b a c a d)
;;(b c a (b a) c a)
;;(b (c d))

;;(occurs 'a '(a b a c a d)) => 3
;;(occurs 'a '(b c a (b a) c)) => 1
;;(occurs 'a '(b (c d))) => 0

(define occurs 
  (lambda (item ls)
    (cond
     ((null? ls) 0)
     ((equal? item (car ls)) (+ 1 (occurs item (cdr ls))))
     (else (occurs item (cdr ls))))))

(define occurs-it
  (lambda (item ls) 
    (letrec
	((occurs-it-help
	  (lambda (item ls acc)
	    (cond
	     ((null? ls) acc)
	     ((equal? item (car ls)) (occurs-it-help item (cdr ls) (add1 acc)))
	     (else (occurs-it-help item (cdr ls) acc))))))
      (occurs-it-help item ls 0))))

;;problem2- 
;;Write a function, calculator, which takes an infix arithmetic
;; expression and evaluates it

;; (calculator 42) => 42
;; (calculator '(1 + 2)) => 3
;; (calculator '(1 + (2 * 8))) => 17
;; (calculator '((((2 + 3) * 2) / 5) + (17 - 1))) => 18

(define calculator 
  (lambda (ls)
    (cond 
     ((number? ls) ls)
     ((pair? (car ls)) (calculator (cons (calculator (car ls)) (cdr ls))))
     ((pair? (caddr ls)) 
      (calculator (cons (car ls)(cons (cadr ls)(list (calculator (caddr ls)))))))
     (else
      (cond 
       ((equal? (cadr ls) '+)
	(+ (car ls) (caddr ls)))
       ((equal? (cadr ls) '-)
        (- (car ls) (caddr ls)))
       ((equal? (cadr ls) '/)
        (/ (car ls) (caddr ls)))
       ((equal? (cadr ls) '*)
        (* (car ls) (caddr ls))))))))

;; problem3-
 
;;Write a function, infix->prefix, which takes an infix
;;arithmetic expression and returns the corresponding prefix expression.

;;(infix->prefix 42) => 42
;;(infix->prefix '(1 + 2)) => (+ 1 2)
;;(infix->prefix '(1 + (2 * 8))) => (+1 (* 2 8))
;;(infix->prefix '((((2 + 3) * 2) / 5) + (17 - 1)))
;; => (+ (/ (* (+ 2 3) 2) 5) (- 17 1))

(define infix->prefix 
  (lambda (ls)
    (cond 
     ((number? ls) ls)
     ((pair? (car ls)) (cons (cadr ls)(cons (infix->prefix (car ls))
					    (list (infix->prefix (caddr ls))))))
     (else (cons (cadr ls) (cons (car ls) (cons (infix->prefix (caddr ls)) '())))))))


;; problem4-

;;Define a function iota-iota that takes an integer i as its
;; argument and returns a list of pairs of integers such that:

;;(iota-iota 1) => ((1 . 1))
;;(iota-iota 2) => ((1 . 1) (1 . 2) (2 . 1) (2 . 2))
;;(iota-iota 3) =>
;; ((1 . 1) (1 . 2) (1 . 3) (2 . 1) (2 . 2) (2 . 3) (3 . 1) (3 . 2) (3 . 3))

;;All helper functions should be tail-recursive and should be defined
;; within the body of iotaiota using letrec.

(define iota-iota
  (lambda (n) 
    (letrec ((iota 
	      (lambda (it1 it2 ls)
		(cond 
		 ((and (= it1 n) (= it2 n)) (reverse (cons (cons it1 it2) ls)))
		 ((= it2 n) (iota (add1 it1) 1 (cons (cons it1 it2) ls)))
		 (else (iota it1 (add1 it2) (cons (cons it1 it2) ls)))))))
      (iota 1 1 '()))))

;; problem5-
;;(digits->number '(7 6 1 5)) => 7615

(define digits->number
  (lambda (ls)
    (letrec ((dig->num
              (lambda (n acc ls)
                (cond
                 ((null? ls) acc)
                 ((zero? n) (dig->num (add1 n) (car ls) (cdr ls)))
                 (else (dig->num (add1 n) (+ acc (* (car ls)(pow-ten n 1)))(cdr ls))))))
	     (pow-ten
	      (lambda (n acc)
		      (if (zero? n) 
			  acc
			  (pow-ten (sub1 n) (* 10 acc)))))) 
      (dig->num 0 0 (reverse ls)))))

;; problem6-
;;(cond->if '(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0)))
;;=> (if (> x y) (- x y) (if (< x y) (- y x) 0))

(define cond->if
  (lambda (ls) 
    (cond
     ((equal? (car ls) 'cond)
      (cons 'if (cons (caadr ls) (cons (cadadr ls)(cond->if (cddr ls))))))
     ((equal? (caar ls) 'else) (cdar ls))    
     (else (cons (cons 'if (cons (caar ls) (cons (cadar ls) (cond->if (cdr ls))))) '())))))


;; problem7-

(define cos
  (lambda (x)
    (letrec
	((help 
	   (lambda (x pow fact factn n acc)
	     (cond 
	      ((= n 100) acc)
	      ((zero? n) (help x (* x x) (* 1 2) 2 (add1 n) 1.0))
	      ((zero? (modulo n 2))
	       (help x (* pow (* x x)) (* fact (* (add1 factn) (+ factn 2))) (+ factn 2) (add1 n)(+ acc (/ pow fact))))
	       (else 
		(help x (* pow (* x x)) (* fact (* (add1 factn) (+ factn 2))) (+ 2 factn) (add1 n) (- acc (/ pow fact)))))))) 
      (help x 0 0 0 0 0) )))