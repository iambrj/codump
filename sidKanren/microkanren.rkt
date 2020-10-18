; http://webyrd.net/quines/quines.pdf
; 'q
; (run (q) (!! SYMBOLS))
; (run (x y) (!! SYMBOLS))
; #(0), #(1)
(define (var x) (vector x))
(define (var? x) (vector? x))
(define (var=? x y) (= (vector-ref x 0) (vector-ref y 0)))

; conj, disj, ==
(define (== u v))

; goals, state
; goal: vars -> bool
; state: assignemnt to vars: { var -> bool }
; QUESTION: exists s: state, goal(s) = true
; state
; 1. Subs
; 2. Counter
; (assp, number) | assp ~ DSU (?)
(define (ext-s x v s)
  (`((,x . ,v) . ,s)))
; ==
; u == v
;; (and #t v) -> v | NICE! 
(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? v u)) s))))
    (if pr (walk (cdr pr) s) u)))
; ((u v), (v, u))
(define (unify u v s)
  (let ([u (walk u s)]
	[v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
        (unify (cdr u) (cdr v) (unify (car u)(car v) s))]
      [else (and (eqv? u v) s)])))

; TODO: What is s?
(define (== u v)
  (lambda (s)
    (let [(s (unify u v (car s)))]
      (if s (unit `(,s . ,(cdr s))) mzero))))

(define (unit s) (cons s mzero))
(define mzero '())

(define (fresh f)
  (lambda (s)
    (let ([c (cdr s)])
      ((f (var c)) `((car s) . ((+ c 1)))))))

(define (disj g1 g2)
  (lambda (s)
    (mplus (g1 s) (g2 s))))

(define (conj g1 g2)
  (lambda (s)
    (bind (g1 s) g2)))

;; bind :: stream a -> ( a-> stream b ) -> stream b
; [a] -> 

; (define (mplus xs ys)
;   (cond
;     [(null? xs) ys]
;     [cons (car xs) (mplus (cdr xs) ys)]))
; 
; (define (bind xs g)
;   (cond
;     [(null? xs) mzero]
;     [else (mplus (g (car xs)) (bind (cdr xs) g))]))

; (define (ones x) (disj (== x 1) (ones x)))
; (define (ones x) 
; (disj (=== x 1)
; (lambda (s) (lambda () (ones x) s))))

(define (mplus g1 g2)
  (cond
    [(null? g1) g2]
    ;; binary trampoline. Nice!
    [(procedure? g1) (lambda () (mplus g2 (g1)))]
    [cons (car g1) (mplus (cdr xs) g2)]))

(define (bind xs g)
  (cond
    [(null? xs) mzero]
    [(procedure? xs) (lambda () (bind (xs) g))]
    [else (mplus (g (car xs)) (bind (cdr xs) g))]))

; disj+, conj+, 
; conde
; [() ()]
; [() ()]

;; (run 1 (p) (fresh (x) (= x p)))
; eval : expr -> val
; (run 1 (q) (eval q q))
; (define (anyo) (== #t #t) (anyo))
;
; *-----------------*
; |TODO for sat/sun?|
; *-----------------*
;
; 1. finish the user facing side, check that our examples work.
; 2. Implement full relational interpreter?
; 3. $$$
