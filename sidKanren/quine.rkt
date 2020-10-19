#lang racket
(require "mk.rkt")
; http://webyrd.net/quines/quines.pdf
; (run 2 (q) (conde [(symbolo q) (== q ''a)]))

; eval : expr -> val
; (run 1 (q) (eval q q))

; cbv lc
; quotes, lists

; symbolo q: enforce q is a symbol.
; absento x y: list x and symbol y
(define (lookupo id env val)
  (conde
    [(== env '()) (== #t #f)]
    [(fresh (x y resto)
       (== `((,x . ,y) . ,resto) env)
       (conde
         [(== x id) (== y val)]
         [(=/= x id) (lookupo id resto val)]))]))

(define (not-in-env sym env)
  (conde
    [(== env '())]
    [(fresh (x y resto)
       (== `((,x . ,y) . ,resto) env)
       (=/= x sym)
       (not-in-env sym resto))]))

;; (define eval-expo
;;   (lambda (expr env val)
;;     (conde [(...ban ridiculous thing from env)
;; 	    (conde symbolo
;; 	      ...
;; 	      )]


(define eval-expo
  (lambda (expr env val)
    (conde
      [(symbolo expr) (lookupo expr env val)]
      [(== expr `(quote ,val))
       (absento 'closure val)
       (not-in-env 'quote env)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (== `(closure ,x ,body ,env) val)
         (symbolo x)
         (not-in-env 'lambda env))]
      [(fresh (rator rand a env2 body x)
         (== `(,rator ,rand) expr)
	 #|
	 (not-in-env 'list env)
	 (not-in-env 'list env2)
	 |#
         (eval-expo rator env `(closure ,x ,body ,env2))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env2) val))]
      [(fresh (e*)
	 (== `(list ,e*) expr)
	 (not-in-env 'list env)
	 (absento 'closure e*)
	 (mapo (lambda (x v) (eval-expo x env v)) e* val))])))

(define (mapo g x* y*)
  (conde
    [(== x* empty) (== y* empty)]
    [(fresh (x xs y ys)
       (== x* (cons x xs))
       (== y* (cons y ys))
       (g x y)
       (mapo g xs ys))]))
; ((lambda (x) (x x)) (lambda (x) (x x)))

; '(closure (x) x ()) (lambda (x) x) ()

((lambda (x)
   (list x (list (quote quote) x)))
 (quote
   (lambda (x)
     (list x (list (quote quote) x)))))

; ((lambda (x)
;    `(,x (quote ,x)))
;  (quote
;    (lambda (x)
;      `(,x (quote ,x)))))

; synthesize a program such that
; (P[i]) = (P[i-1], i, P[i+1])

; (list x ('quote x))


;; (λ x (x x)) (λ x (x x))

; (run 1 (p q) (. p q) (eval o p q) (eval o q p))
