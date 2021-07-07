;;Craig Parry
;;cs357
;;Homework1

;; exercise 1.2

;;(define big-number 10500900)
;;(define small-number 0.00000025)
;;(define cheshire 'cat)
;;(define number1 big-number)
;;(define number2 'big-number

;;a. 10500900
;;b. 2.5e-07
;;c. 'big-number
;;d. 'cat
;;e. 'cheshire
;;f. 10500900
;;g. 'big-number
;;h. 'number1

;;exercise 1.3
;;a. 4
;;b. 2/5
;;c. 2/3
;;d. 0.6666666666666667

;;exercise 1.4
;;a.
;;(- (* 4 7) (+ 13 5))

;;b.
;;(* 3 (+ 4 (- (- 5) (- 3) )))

;;c.
;;(/ 2.5 (* 5 (/ 1 10)))

;;d.
;;(* 5 (+ (* 537 (+ 98.3 (- 375 (* 2.5 153)))) 255))

;;exercise 1.5
;;a. (a + ((b + r)- a))
;;b. ((a * b) + (r * b))
;;c. ((a - b) / (a - r))

;;exercise 1.6
;;a. 
;;(cons 'one (cons 'two (cons 'three (cons 'four '()))))

;;b.
;;(cons 'one (cons (cons 'two (cons 'three (cons 'four '()))) '()))

;;c.
;;(cons 'one (cons (cons 'two (cons 'three '()))(cons 'four '())))

;;d.
;;(cons (cons 'one (cons 'two '())) (cons (cons 'three (cons 'four '())) '()))

;;e.
;;(cons (cons (cons 'one '()) '()) '())

;;exercise 1.10
;;a. (symbol? (cons a b))
;; #f

;;b. (pair? (cons a b))
;; #t

;;c. (null? (cons a b))
;; #f

;;d. (null? (cdr (cons a '())))
;; #t

;;exercise 1.14 

;;a.(symbol? (car '(cat mouse)))
;; #t

;;b. (symbol? (cdr '((cat mouse))))
;;#f

;;c.(symbol? (cdr '(cat mouse)))
;;#f

;;d. (pair? (cons 'hound '(dog)))
;;#t

;;e. (pair? (car '(Cheshire cat)))
;;#f

;;f. (pair? (cons '() '()))
;;#t

;;exercise 2.1

;;returns second item in the list, list must contain 2 or more items
;;(second '(1 2)) => 2
;;(second '((1) (2))) => '(2)
;;(second '(1 2 3 4 5 6)) => 2
(define second
  (lambda (ls)
    (cadr ls)))

;;exercise  2.3

(define make-list-of-two 
  (lambda (item1 item2) 
    (cons item1 (make-list-of-one item2))))
(define make-list-of-one
  (lambda (item)
    (cons item '())))
(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))
;;a.(firsts-of-both '(1 3 5 7) '(2 4 6)) =>  '(1 2)
;;b.(firsts-of-both '((a b) (c d)) '((e f) (g h))) => '((a b) (e f))

;;exercise 2.4
;;The procedure juggle returns a list that is a rearrangement of the
;; input list so that the first element of this list becomes the second,
;; the second element becomes the third, and the third element becomes the first.

;;(juggle '(jump quick spot)) => '(spot jump quick)
;;(juggle '(dog bites man)) => '(man dog bites)

(define juggle
  (lambda (ls)
    (cons (caddr ls) (cons (car ls)
      (cons (cadr ls) '())))))

;;exercise 2.6
;; a,b,c: #t
;; e,f: #f

;;a. (and a (or b e))
;; #t

;;b. (or e (and (not f) a c))
;; #t

;;c. (not (or (not a)(not b)))
;; #t

;; (and (or a f) (not (or b e)))
;; #f

;;exercise 2.7

;;a. (or (symbol? expr) (not (symbol? expr)))
;; #t

;;b. (and (null? expr) (not (null? expr)))
;; #f

;;c. (not (and (or expr #f) (not expr)))
;;#t 

;;d (not (or expr #t))
;;#f


;;exercise 2.10

(define last-item 
  (lambda (ls) 
    (if (null? (cdr ls)) (car ls) (last-item (cdr ls)))))

(define member? 
  (lambda (item ls)
    (if (null? ls) 
	#f
	(if (equal? item (car ls)) 
	    #t
	    (member? item (cdr ls))))))

(define remove-1st 
  (lambda (item ls)
    (if (null? ls) 
	'()
	(if (equal? item (car ls)) (cdr ls) (cons (car ls) (remove-1st item (cdr ls)))))))


;;exercise 2.12
(define mystery 
  (lambda (ls)
    (if (null? (cddr ls))
	(cons (car ls) '())
	(cons (car ls) (mystery (cdr ls))))))

;;The mystery function tests whether the cdr cdr of the input is the empty
;; list. So we must have an input of a two item list becaue the function
;; cddr does not have a safeguard for lists less than two items. Then it
;; reconstructs the list with the last item removed. It recursively 
;; traverses the list until it has reached a point where the cddr is 
;; the empty list and the second to last item of the list is the car of
;; the ls  in the recursive function call.
;; A good name for the function would be remove-last-item

;;exercise 2.13
;;test cases, can be used for each function, results will differ 
;;(subst-1st 'dog 'cat '(my cat is clever)) => '(my dog is clever)
;;(subst-1st 'b 'a '(c a b a c)) => '(c b b a c)
;;(subst-1st '(0) '(*) '((*) (1) (*) (2))) => '((0) (1) (*) (2))
;;(subst-1st 'two 'one '()) => '()

;;(substv-1st '(0) '(*) '((*) (1) (*) (2))) => '((*) (1) (0) (2))
;;(substq-1st '(0) '(*) '((*) (1) (*) (2))) => '((*) (1) (0) (2))

;; substitutes the first occurance in the list of the old item with 
;; the new item 

(define subst-1st 
  (lambda (new old ls)
    (cond 
     ((null? ls) '())
     ((equal? (car ls) old) (cons new (cdr ls)))
     (else (cons (car ls) (subst-1st new old (cdr ls)))))))

(define substq-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((eq? (car ls) old) (cons new (cdr ls)))
     (else (cons (car ls) (subst-1st new old (cdr ls)))))))

(define substv-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((eqv? (car ls) old) (cons new (cdr ls)))
     (else (cons (car ls) (subst-1st new old (cdr ls)))))))
;;exercise 2.14

;;(insert-left-1st 'hot 'dogs '(I eat dogs)) => '(I eat hot dogs)
;;(insert-left-1st 'fun 'games '(some fun)) => '(some fun)
;;(insert-left-1st 'a 'b '(a b c a b c)) => '(a a b c a b c)
;;(insert-left-1st 'a 'b '()) => '()

;; inserts new item to the left of the first instance of the old item
(define insert-left-1st
  (lambda (new old ls)
    (cond 
     ((null? ls) '())
     ((equal? (car ls) old) (cons new ls))
     (else (cons (car ls) (insert-left-1st new old (cdr ls)))))))

;;exercise 2.15

;;(list-of-first-items '((a) (b c d) (e f))) => '(a b e)
;;(list-of-first-items '((1 2 3) (4 5 6))) => '(1 4)
;;(list-of-first-items '((one))) => '(one)
;;(list-of-first-items '()) => '()

;;input must be a list of non-empty list items
(define list-of-first-items
  (lambda (ls)
    (cond
     ((null? ls) '())
     (else (cons (car (car ls)) (list-of-first-items (cdr ls)))))))

;;exercise 2.16

;;(replace 'no '(will you do me a favor)) => '(no no no no no no)
;;(replace 'yes '(do you like ice cream)) =>'(yes yes yes yes yes)
;;(replace 'why '(not)) => '(why)
;;(replace 'maybe '()) => '()

;; replaces top level items of input list with new item
(define replace
  (lambda (item ls)
    (cond 
     ((null? ls) '())
     (else (cons item (replace item (cdr ls)))))))

;;exercise 2.18 

;;(remove-last 'a '(b a n a n a s)) => '(b a n a n s)
;;(remove-last 'a '(b a n a n a )) => '(b a n a n)
;;(remove-last 'a '()) => '()

;; removes last occurance of the speified item in the list
(define remove-last
  (lambda (item ls)
    (cond
     ((null? ls) '())
     ((equal? item (car ls)) 
      (if (is-last item (cdr ls))
	  (cdr ls) 
	  (cons item (remove-last item (cdr ls)))))
      (else (cons (car ls) (remove-last item (cdr ls)))))))

;;(is-last 'a '()) => #t
;;(is-last 'a '(b b b b b )) => #t
;;(is-last 'a '(b b b a)) => #f
;;(is-last 'a '(b a b a)) => #f

;;checks the list for an occurance of the item in the list
(define is-last
  (lambda (item ls)
    (cond 
     ((null? ls) #t)
     ((equal? item (car ls)) #f)
     (else (is-last item (cdr ls))))))