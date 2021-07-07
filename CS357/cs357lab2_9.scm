(define participation 
  (lambda (x y z) 
    (letrec ( 
	     (s (lambda (z x y) (* x (* y z))))
	     (t (lambda (y x z) (* x (* y z))))
	     (v (lambda (y z x) (* x (* y z))))
	     (w (lambda (z y x) (* x (* y z))))
	     )
      (s (t x y z) (v x y z) (w x y z))
      )
    )
)
'(participation 3 7 13)
