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

(define eval-expo
  (lambda (expr env val)
    (conde
      [(symbolo expr) (lookupo expr env val)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (== `(closure ,x ,body ,env) val)
         (symbolo x)
         (not-in-env 'lambda env))]
      [(fresh (rator rand a env2 body x)
         (== `(,rator ,rand) expr)
         (eval-expo rator env `(closure ,x ,body ,env2))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env2) val))])))
;; ((lambda (x) (x x)) (lambda (x) (x x)))


;; (λ x (x x)) (λ x (x x))
