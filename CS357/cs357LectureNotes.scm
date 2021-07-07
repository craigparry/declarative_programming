;;2/5/2020

;;(deep-times '(1 (2) 3)) => 6
;;(deep-times '(1 (2 (((3))))) => 6
(define deep-times
  (lambda (ls)
    (cond ((null? ls) 1) 
	  ((pair? (car ls))
	   (* (deep-times (car ls))
	      (deep-times (cdr ls))))
	  (else
	   (* (car ls)
	      (deep-times (cdr ls)))))))

;;(deep-times '(1 . 2)) => 2
(define deep-times 
  (lambda (sexpr)
    (cond ((null? sexpr) 1)
	  ((pair? sexpr) 
	   (* (deep-times (car sexpr))
	      (deep-times (cdr sexpr))))
	  (else 
	   sexpr))))

;;(reverse '()) => ()
;;(reverse '(1 2 3)) => (3 2 1) 
;;(reverse '(1 (2 3) 4)))=> '(4 (2 3) 1)

(define reverse 
  (lambda (ls) 
    (cond 
     ((null? ls) '())
     ((apprend (reverse (cdr ls))
	       (list (car ls)))))))

;;(append '() '(1 2)) => '(1 2) 
;; (append '(1 2) '(3 4) => (cons 1 (append '(2) '(3 4)))
(define append 
  (lambda (ls0 ls1)
    (if (null? ls0)
	ls1
	(cons (car ls0)
	      (append (cdr ls0) ls1)))))

;; (deep-reverse '(1 (2 3) 4)) => '(4 (3 2) 1)
(define deep-reverse 
  (lambda (ls)
    (cond ((null? ls) '())
	  ((pair? (car ls))
	   (append (deep-reverse (cdr ls))
		   (list (deep-reverse (car ls))))
	   (else 
	    (append (deep-reverse (cdr ls))
		    (list (car ls))))))))

(define evens
  (lambda (ls)
    (if (null? ls)
	'()
	(cons (car ls)
	      (odds ls)))))

(define odds 
  (lambda (ls)
    (if (null? ls)
	'()
	(evens (cdr ls)))))
(define merge 
  (lambda (ls0 ls1)
    (cond 
     ((null? ls0) ls1)
     ((null? ls1) ls0)
     ((> (car ls0) (car ls1))
      (cons (car ls1) (merge ls0 (cdr ls1))))
     (else 
      (cons (car ls0)
	    (merge ls1 (cdr ls0)))))))

(define mergesort
  (lambda (ls) 
    (cond 
     ((null? ls) '())
     ((null? (cdr ls)) ls)
     (else 
      (merge (mergesort (evens ls)) (mergesort (odds ls)))))))

2/7

;; (member? 1 '()) => #f
;; (member? 1 '(1 2 3)) => #t
;; (member? 1 '(2 3 4)) => #f
(define member? 
  (lambda (item ls)
    (if (null? ls) 
	#f
	(or (eq? (car ls) item)
	    (member? item (cdr ls ))))))

(define fib 
  (lambda (x) 
    (if (< x 2)
	x 
	(+ (fib (- x 1)) (fib (- x 2))))))

(define fib-it 
  (lambda (x acc0 acc1)
    (if (= x 0) 
	acc0
	(fib-it (- x 1) acc1 (+ acc0 acc1)))))

(define length
  (lambda (ls) 
    (if (null? ls) 
	0
	(+ 1 (length (cdr ls))))))

(define length-it
  (lambda (ls acc)
    (if (null? ls) 
	acc 
	(length-it (cdr ls) (+ acc 1)))))

(define reverse 
  (lambda (ls) 
    (if (null? ls) 
	'()
	(append (reverse (cdr ls)) 
		(list (car ls))))))

(define append 
  (lambda (ls0 ls1)
    (if (null? ls0)
	ls1
	(cons (car ls0)
	      (append (cdr ls0) ls1)))))
;; ls       acc
;;(3 2 1)   ()
;;(2 1)     (3)
;;(1)       (2 3)
;;()        (1 2 3)

(define reverse-it 
  (lambda (ls acc) 
    (if (null? ls)
	acc
        (reverse-it (cdr ls) (cons (car ls) acc)))))


;;(let ((x 1) (y 2)) (+ x y))
;;syntactic sugar for 
;; ((lambda (x y) (+ x y)) 1 2)

(define append 
  (lambda (ls0 ls1)
    (reverse-it (reverse-it ls0 '()) ls1)))

(define append 
  (lambda (ls0 ls1)
    (letrec
	((loop
	  (lambda (ls acc)
	    (if (null? ls)
		acc
		(loop (cdr ls) (cons (car ls) acc))))))
    (loop (loop ls0 '()) ls1))))

;;2/10 
;; no class wednesday 
;; natural numbers 0,1,2,3,4,...
;; Peano : a natural number can be of two forms, zero, or a successor of a natural number 

;; Introduce a data type of natural numbers, nat
;; Representation as a list of ....

;; read Z aas Zero 
(define Z '())

;; read S as succesor succ 
(define S 
  (lambda (x) 
    (cons '() x )))

(define nat_is_zero? null?)

;; read pred as predecessor 
(define nat_pred cdr)

;;---------------------------------------------------------------------------
;; everything below uses only the interface: 
;; nat constructors Z and S
;; and the functions nat_is_zero? and nat_pred
;;---------------------------------------------------------------------------

(define number->nat
  (lambda (k) ;; k >=0
    (if (zero? k) 
	Z
	(S (number->nat (sub1 k))))))

(define nat->number
  (lambda (x) 
    (if ( nat_is_zero? x) 
	0
	(add1 (nat->number (nat_pred x))))))

(define nat_smaller? 
  (lambda (x y) 
    (cond ((nat_is_zero? x) (not (nat_is_zero? y)))
	  ((nat_is_zero? y) #f)
	  (else 
	   (nat_smaller? (nat_pred x) (nat_pred y))))))

;; addition is iterated counting 
;; x + 0 = x 
;; x + y = 1+ (x+ (y-1))

(define nat_plus
  (lambda (x y) 
    (if (nat_is_zero? y) 
	x
	(S (nat_plus x (nat_pred y)))))) 

;;multiplication is iterated addition 
;; x * 0 = 0
;; x * y = x *x*(y-1)

(define nat_times 
  (lambda (x y) 
    (if (nat_is_zero? y) 
	Z
	(nat_plus x (nat_times x (nat_pred y))))))

;;exponentiation is iterated multiplication 
;; x^0 = 1
;;x^y = x* x^(y-1)
(define nat_power
  (lambda (x y) 
    (if (nat_is_zero? y) 
	(S Z) 
	(nat_times x (nat_power x (nat_pred y))))))

;;tetration is iterated exponentiation 
;;x^^0 = 1
;;x^^y = x ^ (x^^(y-1)
;;2^^4 = 2 ^ (2 ^ (2 ^ 2)) = 65536
(define nat_tetration 
  (lambda (x y) 
    (if (nat_is_zero? y) 
	(S Z) 
	(nat_power x (nat_tetration x (nat_pred y))))))

;; pentation is iterated tetration 
(define nat_pentation 
  (lambda (x y) 
    (if (nat_is_zero? y) 
	(S Z) 
	(nat_tetration x (nat_pentation x (nat_pred y))))))

;;2^^^3 = ?why is this 65536 (predecessor is 2^^4)
;;3^^^2 = 3^27 too big 

;;Use recursion to generalize the up-arrow notation to any number of ^ arrows 
;; x^n y = x^y if n=1
;;x ^n y =1 for all n, y =0
;; x ^n y = x ^(n-1) (x ^n (y-1)) 

(define nat_up_arrow 
  (lambda (n x y) 
    (cond ((= n 1) (nat_power x y))
	  ((nat_is_zero? y) (S Z))
	  (else (nat_up_arrow (sub1 n) x (nat_up_arrow n x (nat_pred y)))))))

(define nat_up_arrow_alt
  (lambda (n)
    (if (= n 1) 
	nat_power
	(lambda (x y) 
	  (if (nat_is_zero? y) 
	      (S Z) 
	      ((nat_up_arrow_alt (sub1 n)) x ((nat_up_arrow_alt n) x (nat_pred y))))))))



;;2/14/2020

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define iterate 
  (lambda (f n) 
    (if (zero? n)
	(lambda (x) x)
	(compose f (iterate f (sub1 n))))))

(define list-ref
  (lambda (ls n) 
    (car ((iterate cdr n) ls))))

;; extract squre roots iteratively 
;; guess ------->(guess + n/guess)/2

(define improve 
  (lambda (x)
    (lambda (guess)
      (/ (+ guess (/ x guess)) 2))))

(define square-root
  (lambda (x)
    ((iterate (improve x) 20) x)))


;;2/17                                                                                  

;;0 -'()                                                                                
;;1 -'(())                                                                              
;;2 -'(() ())                                                                           
;;3 -'(() () ())                                                                        

(define zilch '())
(define zilch? null?)

(define succ
  (lambda (x) (cons'() x)))

;; 0 - '()                                                                              
;; 1 -'(())                                                                             
;; 2 -'((()))                                                                           
;; 3 -'(((())))                                                                         

(define zilch '())
(define succ
  (lambda (x) (cons x'()))
(define pred car)

;; 0 - '()                                                                              
;; 1 - (lambda () '())                                                                  
;; 2 - (lambda () (lambda () '()))                                                      
;; 3 - (lambda () (lambda () (lambda () '())))                                          

(define z

(define pred
  (lambda (x) (x)))

(define succ
  (lambda (x) (lambda () x)))


;;(define pred                                                                          
;;  (lambda (x) (cdr x)))                                                               
;; or write                                                                             
(define pred cdr)

(define number->nat
  (lambda (k)
    (if (zero? k)
        zilch
        (succ (number->nat (sub1 k))))))

(define nat->number
  (lambda (x)
    (if (zilch? x)
        0
        (add1 (nat->number (pred x))))))


;; x < y = x - 1 < y -1                                                                 
(define smaller?
  (lambda (x y)
    (cond ((zilch? x) (not (zilch? y)))
          ((zilch? y) #f)
          (else
           (smaller? (pred x) (pred y))))))

;; x - 0 = x                                                                            
;; x - y = (x - 1) - (y - 1)                                                            
(define minus
  (lambda (x y)
    (if (zilch? y)
        x
        (minus (pred x) (pred y)))))

;; x + 0 = x                                                                            
;; x + y = (x + 1) + (y -1)                                                             
(define plus
  (lambda (x y)
    (if (zilch? y)
        x
        (plus (succ x) (pred y)))))

;; x * 0 = 0                                                                            
;; x * 1 = x                                                                            
;; x * y = x * (y - 1) + x                                                              
(define times
  (lambda (x y)
    (if (zilch? y)
        zilch
        (plus x (times x (pred y))))))

(define one (succ zilch))


(define one (succ zilch))

;; x^0 = 1                                                                              
;; x^1 = x                                                                              
;; x^y = x^(y-1) * x                                                                    
(define power
  (lambda (x y)
    (cond ((zilch? x) zilch)
     ((zilch? y) one)
     ((zilch? (pred y)) x)
     (else
      (times x (power x (pred y)))))))






;; lab 2/17                                                                             
(define Z
  (lambda (f x)
    x
    )
)

;; ((S(S(S Z))) add1 0)                                                                 
;; n => lam (f x) (f(f(...(f x))...))                                                   
(define S
  (lambda (fx)
    (lambda (f x) (f (fx f x)))

  )
)

;;2/19

;; ax^2 + bx + c
(define eval-quadratic
  (lambda (a b c x) 
    (+ (* a x x) (* b x) c)))

;; (eval-quadratic 1 2 3 15)

(define eval-quadratic-c
  (lambda (a b c) 
    (lambda (x)
      (+ (* a x x) (* b x) c))))

;; ((eval-quadratic-c 1 2 3) 15)

;; (-b +/- sqrt (b^2 - 4ac))/2a)
(define quadratic1
  (lambda (a b c)
    (list (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) 
	     (* 2 a))
	  (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
             (* 2 a)))))

(define quadratic2
  (lambda (a b c)
    ((lambda (discriminant denominator -b) (list (/ (+ -b discriminant) denominator)
						 (/ (- -b discriminant) denominator)))
     (sqrt (- (* b b) (* 4 a c)))
     (* 2 a)
     (- b))))

(define quadratic3
  (lambda (a b c)
    (let ((discriminant (sqrt (- (* b b) (* 4 a c))))
	  (denominator (* 2 a))
	  (-b (- b)))
      (list (/ (+ -b discriminant) denominator)
	    (/ (- -b discriminant) denominator)))))

;; (let ((x 2) (y 3)) (+ x y)) => ((lambda (x y) (+ x y)) 2 3)


(define fact-helper
  (lambda (x acc)
    (if (= x 0)
	acc
	(fact-helper (- x 1) (* x acc)))))

(define fact 
  (lambda (x)
    (fact-helper x 1)))

(define fact 
  (lambda (x)
    (letrec 
	((loop
	  (lambda (x acc)
	    (if (= x 0)
		acc
		(loop (- x 1) (* x acc))))))
      (loop x 1))))
(define append 
  (lambda (ls0 ls1) 
    (letrec 
	((loop
	  (lambda (ls acc) 
	    (if (null? ls) 
		acc
		(loop (cdr ls) (cons (car ls) acc))))))
      (loop (loop ls0 '()) ls1))))


;; (let ((x 2) (y 3)) (+ x y)) => ((lambda (x y) (+ x y)) 2 3) 

;; (bindings '(let ((x 2) (y 3)) (+ x y))) => ((x 2) (y 3))
(define bindings cadr)

;;(vars-helper '((x 2) (y 3))) => (x y)
;;(vars-helper '((x 2) (y 3) (z 4))) => (x y z)
(define vars-helper
  (lambda (bindings)
    (if (null? bindings)
	'()
	(cons (caar bindings)
	      (vars-helper (cdr bindings))))))
(define body caddr)
;;(vars '(let ((x 2) (y 3)) (+ x y))) =
(define vars 
  (lambda (expr)
    (vars-helper (bindings expr))))
;;(body '(let ((x 2) (y 3)) (+ x y))) => (+ x y)

(define vars-helper
  (lambda (bindings)
    (if (null? bindings)
	'()
	(cons (cadar bindings)
	      (vars-helper (cdr bindings))))))

(define vals-helper
  (lambda (bindings)
    (if (null? bindings)
	'()
	(cons (cadar bindings)
	      (vals-helper (cdr bindings)))))

(define vals 
  (lambda (expr)
    (vals-helper (bindings expr))))

(define let->lambda 
  (lambda (expr)
    (append (list (list 'lambda (vars expr) (body expr))) (vals expr))))
;;2/21

(define add1-to-all
  (lambda (ls)
    (if (null? ls)
	'()
	(cons (add1 (car ls))
	      (add1-to-all (cdr ls))))))

(define sqr-all
  (lambda (ls)
    (if (null? ls)
	'()
	(cons ((lambda (x) (* x x)) (car ls))
	      (sqr-all (cdr ls))))))

(define map 
  (lambda (proc ls) 
    (if (null? ls) 
	'()
	(cons (proc (car ls)) 
	      (map proc (cdr ls))))))

(define vars-helper 
  (lambda (bindings) 
    (map caar bindings)))

(define vals-helper
  (lambda (bindings) 
    (map cadar bindings)))

(define add1-all
  (lambda (ls) 
    (map add1 ls)))

(define sqr-all
  (lambda (ls) 
    (map (lambda (x) (* x x)) ls)))

(define map-c
  (lambda (proc ls)
    (lambda (ls)
      (if(null? ls)
         '()
         (cons (proc (car ls))
                   (map-c proc (cdr ls)))))))

(define vars-helper (map-c caar))

(define vals-helper (map-c cadar)

(define add1-all (map-c add1))

(define sqr-all (map-c (lambda (x) (* x x))))

;; (kills-evens '(1 2 3 4)) => '(1 3)

(define kills-evens
  (lambda (ls) 
    (cond ((null? ls) '())
	  ((even? (car ls)) (kills-evens (cdr ls)))
	  (else 
	   (cons (car ls) (kill-evens (cdr ls)))))))

(define kills-booleans
  (lambda (ls)
    (cond ((null? ls) '())
          ((boolean? (car ls)) (kills-booleans (cdr ls)))
          (else
           (cons (car ls) (kill-booleans (cdr ls)))))))

(define delete
  (lambda (item ls)
    (cond ((null? ls) '())
          ((eq? (car ls)) (delete item (cdr ls)))
          (else
           (cons (car ls) (delet item (cdr ls)))))))

(define filter-c
  (lambda (pred)
    (lambda (ls)
      (cond ((null? ls) '())
	    ((pred (car ls)) ((filter-c pred) (cdr ls)))
	  
	    (else
	     (cons (car ls) ((filer-c pred) (cdr ls))))))))

(define kill-evens (filter-c even?))
(define kill-booleans (filer-c boolean? ))

(define delete 
  (lambda (item ls)
    ((filter-c (lambda (x) (eq? x item))) ls)))

;;(eq? 1 2) => #f
;;((eq?-c 1) 2) => #f
(define eq?-c
  (lambda (x) 
    (lambda (y) (eq? x y))))

(define delete 
  (lambda (item ls) 
    ((filter-c (eq?-c item)) ls)))

;;(iota 7) => (1 2 3 4 5 6 7)

(define iota 
  (lambda (n)
    (letrec 
	((loop
	  (lambda (n acc)
	    (if (= n 0)
		acc
		(loop (- n 1) (cons n acc))))))
      (loop n '()))))

(define fact 
  (lambda (n) 
    (apply * (iota n))))


;; e = 1/0! + 1/1! + 1/2! + 1/3!

;;(iota 100) => (1 2 3...100) => (0 1 2 3...99) => (0! 1! 2!... 99!)

(define e (apply + (map / (map fact (map sub1 (iota 100))))))

(define max 
  (lambda args
    (letrec 
	((singleton? 
	  (lambda (ls)
	    (null? (cdr ls))))
	 (max2
	  (lambda (x y)
	    (if (> x y) x y)))
	 (max-list
	  (lambda (ls)
	    (if (singleton? ls)
		(car ls)
		(max2 (car ls)
		      (max-list (cdr ls)))))))
      (max-list args))))

;; improved 

(define max
  (lambda args
    (letrec
	((singleton?
          (lambda (ls)
            (null? (cdr ls))))
	  (max2
          (lambda (x y)
            (if (> x y) x y)))
	   (max-list
          (lambda (ls)
            (if(singleton? args)
	       (car args)
	       (max2 (car args)
		           (apply max (cdr args)))))))
(max args))))

(define max 
  (lambda (first . rest)
    (letrec 
	((max2
	  ((lambda (x y)
	     (if (> x y) x y))))
	 (if (null? rest)
	     first 
	     (max2 first
		   (apply max rest)))))))

;;2/24

(define round-n-places
  (lambda (x n) 
    (let ((pot (expt 10.0 n)))
	  (/ (floor (+ (* x pot) 0.5)) pot))))

(define round-n-places-c
  (lambda (n)
    (lambda (x)
      (let ((pot (expt 10.0 n)))
          (/ (floor (+ (* x pot) 0.5)) pot)))))

(define foo 
  (lambda (w x y z)
    (if (= w x)
	y
	z)))

(define bar 
  (lambda (w) 
    (lambda (x) 
      (lambda (y)
	(lambda (z) 
	  (if (= w x)
	      y
	      z))))))

(define fred
  (lambda (a b c d w x y z)
    (apply + (map * (list a b c d) (list w x y z)))))

(define wilma
  (lambda (a b c d)
    (lambda (w x y z)
      (apply + (map * (list a b c d) (list w x y z))))))

(define dot
  (lambda v1 
    (lambda v2 
      (apply + (map * v1 v2)))))

(define add-all
  (lambda (ls)
    (if (null? ls)
	0
	(+ (car ls) 
	   (add-all (cdr ls))))))

(define sqrt-all
  (lambda (ls) 
    (if (null? ls)
	'()
	(cons (sqrt (car ls))
	      (sqrt-all (cdr ls))))))

(define length 
  (lambda (ls) 
    (if (null? ls)
	0 
	(+1 (length (cdr ls))))))

(define flat-recur
  (lambda (seed list-proc) 
    (lambda (ls) 
      (if (null? ls)
	  seed 
	  (list-proc (car ls)
		     ((flat-recur seed list-proc) (cdr ls)))))))

;;((flat-recur '() (lambda (x y) (cons (sqrt x) y))) '(1 2 2 3 4))
;;'(1 1.4142135623730951 1.4142135623730951 1.7320508075688772 2)


(define add-all (flat-recur 0 +))

(define sqrt-all (flat-recur '() (lambda (x y) (cons (sqrt x) y))))

(define length (flat-recur 0 (lambda (x y) (+ y 1))))

(define flat-recur
  (lambda (seed list-proc)
    (letrec
	((pattern
	  (lambda (ls)
	    (if (null? ls)
		seed 
		(list-proc (car ls)
			   (pattern (cdr ls)))))))
pattern)))

(define map 
  (lambda (proc ls)
    (if (null? ls) 
	'()
	(cons (proc (car ls)) 
		    (map proc (cdr ls))))))

(define map-c
  (lambda (proc)
    (lambda (ls)
      (if (null? ls)
	 '()
	 (cons (proc (car ls))
                   ((map-c proc) (cdr ls)))))))

(define map 
  (lambda (proc) 
    (flat-recur '() (lambda (x y) (cons (proc x) y)))))


;;2/26

(define wrap 
  (lambda (x) 
    (if (pair? x) 
	x 
	(list x))))
;;(flatten '(1 (2 3) (4) 5)) => (1 2 3 4 5)
;;(flatten '(1 ((((2 3)))) (4) 5)) => (1 (((2 3))) 4 5)

(define flatten 
  (lambda (ls) 
    (if (null? ls) 
	'()
	(append (wrap (car ls)) (flatten (cdr ls))))))

(define flatten (flat-recur '() (lambda (x y) (append (wrap x) y))))

;;(flatten-all '(1 ((((2 3)))) (4) 5))=> (1 2 3 4 5)
(define flatten-all
  (lambda (ls)
    (cond ((null? ls) '())
	  ((pair? (car ls))
	   (append (flatten-all (car ls))
		   (flatten-all (cdr ls))))
	  (else 
	   (cons (car ls)
		 (flatten-all (cdr ls)))))))
;; (reverse '(1 2 3))=> (3 2 1)
;; (reverse '(1 (2 3) 4)) => (4 (2 3) 1)

(define reverse 
  (lambda (ls)
    (if (null? ls)
	'()
	(append (reverse (cdr ls))
		(list (car ls))))))

;; (reverse-all '(1 (2 3) 4)) => (4 (3 2) 1)
(define reverse-all
  (lambda (ls)
    (cond ((null? ls) '())
	  ((pair? (car ls))
	   (append (reverse-all (cdr ls))
		   (list (reverse-all (car ls)))))
	  (else 
	   (append (reverse-all (cdr ls))
		   (list (car ls)))))))


;;(map flatten-all (1 (2 3) ((4)) 5)) => '((1) (2 3) (4) (5))=>
;;(flatten '((1) (2 3) (4) (5)) => (1 2 3 4 5)
(define flatten-all
  (lambda (ls) 
    (if (pair? ls)
	(flatten (map flatten-all ls))
	ls)))

(define reverse-all
  (lambda (ls)
    (if (pair? ls)
	(reverse (map reverse-all ls))
	ls)))

(define make-deep
  (lambda (proc)
    (lambda (ls) 
      (if (pair? ls)
	  (proc (map (make-deep proc) ls))
	  ls))))

(define flatten-all (make-deep flatten))
(define reverse-all (make-deep reverse))

(define make-deep
  (lambda (proc) 
    (letrec 
	((pattern
	  (lambda (ls)
	    (if (pair? ls) 
		(proc (map pattern ls))
		ls))))
	 pattern)))


;; (scramble '(a b c)) => (b a c)
;; (scramble '(1 + 3)) => (+ 1 3) 

(define scramble
  (lambda (ls) 
    (list (cadr ls) 
	  (car ls) 
	  (caddr ls))))

(define infix->prefix (make-deep scramble))

(define prefix-calculator
  (make-deep
   (lambda (ls) 
     (let ((op (car ls)))
       (apply
	(cond ((eq? op '+) +)
	      ((eq? op '-) -)
	      ((eq? op '*) *)
	      (else /))
	(cdr ls))))))


(define compose2 
  (lambda (f g) 
    (lambda (x) (f (g x)))))

(define fold 
  (lambda (proc seed) 
    (lambda (ls) 
      (if (null? ls) 
	  seed 
	  (proc (car ls) 
		((fold proc seed) (cdr ls)))))))

(define compose
  (lambda args
    ((fold compose2 (lambda (x) x)) args)))

;;2/28

;;3/2

(define map
  (lambda (proc ls) 
    (if (null? ls)
	'()
	(cons (proc (car ls)) 
	      (map proc (cdr ls))))))

(define delete 
  (lambda (item ls) 
    (cond ((null? ls) '())
	  ((eq? (car ls) item)
	   (delete item (cdr ls)))
	  (else
	   (cons item (delete item (cdr ls)))))))

(define length
  (lambda (ls) 
    (if (null? ls) 
	0
	(add1 (length (cdr ls))))))

(define append 
  (lambda (ls0 ls1) 
    (if (ull? ls0)
	ls1
	(cons (car ls0) 
	      (append (cdr ls0) ls1)))))

;;write them curried 

(define map-c
  (lambda (proc)
    (lambda (ls) 
      (if (null? ls)
	  '()
	  (cons (proc (car ls))
		((map-c proc) (cdr ls)))))))

(define delete-c
  (lambda (item) 
    (lambda (ls) 
      (cond ((null? ls) '())
	    ((eq? (car ls) item) 
	     (delete-c item (cdr ls)))
	    (else 
	     (cons item ((delte-c item (cdr ls)))))))))

(define append-c 
  (lambda (ls1) 
    (lambda (ls0) 
      (cond ((null? ls0)
	     ls1
	     (cons (car ls0) 
		   ((append-c ls1) (cdr ls0))))))))


;;map-c, delete-c, length and append-c abstracted as fold 

(define fold 
  (lambda (seed proc)
    (lambda (ls) 
      (if (null? ls) 
	  seed 
	  (proc (car ls) 
		((fold proc seed) (cdr ls)))))))

(define length
  (fold (lambda (x y) (+ x y)) 0))

(define map-c
  (lambda (proc) 
    (fold (lambda (x y) (cons (proc x) y))  '())))

(define delete-c 
  (lambda (item) 
    (fold (lambda (x y) (if (equal? x item)
			    y
			    (cons x y)))'())))

(define append-c 
  (lambda (ls1) 
    (fold cons ls1)))

(define append
  (lambda args 
    ((fold append2 '())args )))

(define append2
  (lambda (ls0 ls1)
    ((append-c ls1)ls0)))

(define max2
  (lambda (x y) 
    (if (< x y) y x)))
    
(define max
  (lambda (x . args)
    ((fold max2 x) args)))