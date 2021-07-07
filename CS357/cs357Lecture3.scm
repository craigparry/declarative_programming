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

(define nat_iterate
  (lambda (f n)
    (if (nat_is_zero? n)
	(lambda (x) x)
	(compose f (nat_iterate f (nat_pred n))))))

(define nat_addition
  (lambda (x y)
    ((nat_iterate S y)
     x)))

(define nat_multiplication 
  (lambda (x y)
    ((nat_iterate (lambda (z) (nat_addition x z)) y) 
		  Z)))

(define nat_exponentiation
  (lambda (x y)
    ((nat_iterate (lambda (z) (nat_multiplication x z)) y)
     (S Z))))

(define nat_Knuth_up_arrow 
  (lambda (n)
    (if (= n 1) 
	nat_exponentiation 
	(lambda (x y) 
	  ((nat_iterate (lambda (z) ((nat_Knuth_up-arrow (sub1 n)) x z)) y)
	   (S Z))))))

(define nnn
  (lambda (n) 
    ((iterate (lambda (nat_function)
		(lambda (x y) 
		  ((nat_iterate (lambda (z) (nat_function (x z)) y) 
				(S Z))))
		(sub1 n))
	      nat_exponentiation))))