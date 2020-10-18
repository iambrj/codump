#lang racket

(require "mk.rkt")

; ==
; fresh
; conde
; run

; conso
(define (conso a d l)
  (== (cons a d) l))
; nullo
(define (nullo l)
  (== l '()))
; caro
; car l =?= out;
; exists cdrv, 
;    cons out cdrv = l
(define (caro2 l out)
  (fresh (cdrv)
  (conso out cdrv l)))

;; exists a, d
;   (cons a d) = l /\ a = out
(define (caro l out)
  (fresh (a d)
  (conso a d l)
  (== a out)))
; cdro
; cdro l out
; cdr l =?= out;
; exists carv,
;   cons carv l = out
(define (cdro1 l out)
  (fresh (carv)
  (conso carv l out)))
(define (cdro l out)
  (fresh (carv)
  (conso carv out l)))
; pairo
(define (pairo p)
  (fresh (x y)
    (conso x y p)))
; listo
(define successful (== #t #t))
(define unsuccessful (== #f #t))
(define (listo l)
  (conde
    [(== l '())]
    [(pairo l)
     (fresh (d)
       (cdro l d)
       (listo d))]
    [(== #t #f)]))
(define listo1               
  (lambda (l)
    (conde
      [(nullo l) successful]
      [(pairo l)
       (fresh (d)
         (cdro l d)
         (listo d))]
      [unsuccessful])))
; membero
(define (membero m l)
  (conde
    [(== l '()) unsuccessful]
    [(fresh (l2) (== l (cons m l2)))]
    [(fresh (x l2) (== l (cons x l2)) (membero m l2))]
    [(== #t #f)]))
; appendo
