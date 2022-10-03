(import (recbase))
(import (scheme))

;if~ a b 0 => a
;if~ a b !0 => b
(define if~ (primitive-rec (p 2 2) (p 4 1)))
;+~ a b => a+b
(define +~ (primitive-rec (p 1 1) (comp s (p 3 3))))
;pre~ a => a-1 if a > 0 else 0
(define pre~ (comp (primitive-rec z (p 3 2)) z (p 1 1)))
;-~ a b => a-b if a >= b else 0
(define -~ (primitive-rec (p 1 1) (comp pre~ (p 3 3))))
;not~ 0 => !0
;not~ !0 => 0
(define not~ (comp (primitive-rec s (comp z (p 3 1))) z (p 1 1)))
;and 0 0 => 0
;and 0 !0 => 0
;and !0 0 => 0
;and !0 !0 => !0
(define and~ (primitive-rec z (p 3 1)))
;or 0 0 => 0
;or 0 !0 => !0
;or !0 0 => !0
;or !0 !0 => !0
(define or~ (primitive-rec (p 1 1) (comp s (p 3 1))))
;xor 0 0 => 0
;xor 0 !0 => !0
;xor !0 0 => !0
;xor !0 !0 => 0
(define xor~ (primitive-rec (p 1 1) (comp not~ (p 3 1))))
;>=~ a b => !0 if a >= b else 0
(define >=~ (primitive-rec s (comp -~ (p 3 1) (p 3 2))))
;>~ a b => !0 if a > b else 0
(define >~ (primitive-rec (p 1 1) (comp -~ (p 3 1) (comp s (p 3 2)))))
;<~ a b => !0 if a < b else 0
(define <~ (primitive-rec z (comp -~ (comp s (p 3 2)) (p 3 1))))
;<=~ a b => !0 if a <= b else 0
(define <=~ (comp >=~ (p 2 2) (p 2 1)))
;!=~ a b => !0 if a != b else 0
(define !=~ (comp or~ -~ (comp -~ (p 2 2) (p 2 1))))
;=~ a b => !0 if a = b else 0
(define =~ (comp not~ !=~))

;min~ a b => b if a > b else a
(define min~ (comp if~ (p 2 1) (p 2 2) (comp -~ (p 2 2) (p 2 1))))
;max~ a b => b if a < b else a
(define max~ (comp if~ (p 2 1) (p 2 2) (comp -~ (p 2 1) (p 2 2))))
;min3 a b c => a if a < b && a < c else b if b < a && b < c else c
(define min3~ (comp min~ (p 3 3) (comp min~ (p 3 1) (p 3 2))))
;max3 a b c => a if a > b && a > c else b if b > a && b > c else c
(define max3~ (comp max~ (p 3 3) (comp max~ (p 3 1) (p 3 2))))

;(define min-n+1~ (comp min~ (p n+1 n+1) (comp min~ (p n+1 1) (p n+1 2) ... (p n+1 n))))

;*~ a b => a*b
(define *~ (primitive-rec z (comp +~ (p 3 1) (p 3 3))))
;pow~ a b => a**b
(define pow~ (primitive-rec (comp s z) (comp *~ (p 3 1) (p 3 3))))

;/~ a b => a/b
(define /~ (comp
	     (primitive-rec 
	       (comp z (p 2 1))
	       (comp if~
		     (comp s (p 4 3))
		     (p 4 4)
		     (comp <=~ (comp *~ (p 4 2) (comp s (p 4 3))) (p 4 1))))
	     (p 2 1)
	     (p 2 2)
	     (p 2 1)))

;%~ a b => a mod b
(define %~ (comp -~ (p 2 1) (comp *~ /~ (p 2 2))))

;root~ a b n => x if x**n <= a && (x+1)**n > a
(define root-v1~ (comp
		(primitive-rec 
		  (comp z (p 2 1))
		  (comp if~
			(comp s (p 4 3))
			(p 4 4)
			(comp <=~ (comp pow~ (comp s (p 4 3)) (p 4 2)) (p 4 1))))
		(p 2 1)
		(p 2 2)
		(p 2 1)))

(define root-v2~ (comp
		 (primitive-rec 
		   (p 3 1)
		   (comp if~
			 (comp -~ (p 5 3) (comp s (p 5 4)))
			 (p 5 5)
			 (comp >~ (comp pow~ (comp -~ (p 5 3) (p 5 4)) (p 5 2)) (p 5 1))))
		 (p 2 1)
		 (p 2 2)
		 (comp s (p 2 1))
		 (comp s (p 2 1))))

(define root-v3~ (comp
		 (primitive-rec
		   z
		   (comp if~
		     (comp s (p 3 3))
		     (p 3 3)
		     (comp <=~ (comp pow~ (comp s (p 3 3)) (p 3 1)) (comp s (p 3 2)))))
		 (p 2 2)
		 (p 2 1)))

;log~ a b => x if b**x <= a && b**(x+1) > a
(define log-v1~ (comp
	       (primitive-rec 
		 (comp z (p 2 1))
		 (comp if~
		       (comp s (p 4 3))
		       (p 4 4)
		       (comp <=~ (comp pow~ (p 4 2) (comp s (p 4 3))) (p 4 1))))
	       (p 2 1)
	       (p 2 2)
	       (p 2 1)))

(define log-v2~ (comp
		(primitive-rec
		  z
		  (comp if~
		    (comp s (p 3 3))
		    (p 3 3)
		    (comp <=~ (comp pow~ (p 3 1) (comp s (p 3 3))) (comp s (p 3 2)))))
		(p 2 2)
		(p 2 1)))


;test cases
(time (display (call min~ 7 9)))
(newline)
(time (display (call min~ 9 7)))
(newline)
(time (display (call max~ 7 9)))
(newline)
(time (display (call max~ 9 7)))
(newline)
(time (display (call min3~ 10 20 30)))
(newline)
(time (display (call min3~ 20 10 30)))
(newline)
(time (display (call min3~ 20 30 10)))
(newline)
(time (display (call root-v1~ 17 4)))
(newline)
(time (display (call root-v2~ 82 5)))
(newline)
(time (display (call root-v3~ 82 5)))
(newline)
;It maybe takes half an hour!
(time (display (call log-v1~ 20 2)))
(newline)
(time (display (call log-v2~ 82 4)))
(newline)
(time (display (call log-v2~ 100 4)))
(newline)
