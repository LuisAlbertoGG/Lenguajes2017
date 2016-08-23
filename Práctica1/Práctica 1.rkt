#lang plai
;ejerccio #1

(define (ec-lin a b)
  display (/(- b )a))

(test (ec-lin 3 2) (-(/ 2 3)))
(test (ec-lin 5 5) -1)
(test (ec-lin 4 3) (-(/ 3 4)))
(test (ec-lin 3 4) (-(/ 4 3)))
(test (ec-lin 2 5) (-(/ 5 2)))
;ejerccio #

(define (area-heron a b c)
  (let ([s (/(+ a b  c) 2)]
        ) 
    display (sqrt
                  (* s (- s a) (- s b )(- s c)))
    )
  )
(test (area-heron 3 25 26) 36)
(test (area-heron 5 7 10) 16.24)
(test (area-heron 10 15 20) 72.61)
(test (area-heron 5 25 26) 62.16)
(test (area-heron 12 34 23) 66.8)

;ejerccio #3

(define (triangulo-rec? a b c)
  (cond
    [(and (> a b) (> a c) ) (= (expt a 2)(+ (expt b 2)(expt c 2)))  ]
    [(and (> b a) (> b c) ) (= (expt b 2)(+ (expt a 2)(expt c 2))) ]
    [ display (= (expt c 2)(+ (expt b 2)(expt a 2))) ]
    
    )
  )
  
;ejerccio #4


;5 - Inversión de números

(define (invierte n)
 (define (loop num reversa)
    (cond
      [(zero? num) reversa]
      [(let ((ultimo (modulo num 10)))
         (loop (/ (- num ultimo) 10) (+ (* reversa 10) ultimo)))]))
   (loop n 0))
   
;6 - Eliminar duplicados

(define (elimina-dup l)
  (cond
    [(empty? l) '()]
    [(empty? (cdr l)) (car l)]
    [(equal? (first l) (second l)) (elimina-dup (cdr l))]
    [else (cons (car l) (elimina-dup (cdr l)))]))


;7 a
(define (bin a)
  (cond
    [(< a 2) (number->string a)]
    [else (string-append(bin(quotient a 2)) (number->string (modulo a 2)))]
        ))


(define (binarios l)
  (cond
    [(empty? l) '()]
    [else (cons (bin (car l)) (binarios (cdr l)))]
        ))
;7 b
(define (primo a b)
  (cond
    [(> 2 a) null]
    [(> 2 b) a]
    [(= (modulo a b) 0) null]
    [else (primo a (- b 1))]
    ))

(define (primos l)
  (cond
    [(empty? l) null]
    [else (remove '() (cons (primo (car l) (- (car l) 1)) (primos (cdr l))))]
    ))
;7 c
(define (reversar l)
  (cond
    [(empty? l) '()]
    [(foldr cons (list(car l)) (reversar(cdr l)))]
    ))

(define (reversal l)
  (cond
    [(empty? l) '()]
    [(foldl cons '() l)]
    ))

;8a - Concatenación de dos listas
(define (concatena l1 l2)
  (cond ((empty? l1) l2)
        ((empty? l2) l1)
        (else (cons (car l1) (concatena (cdr l1) l2)))))

;8b - Funcion a la lista
(define (mapea f l)
  (cond
    [(empty? l) l]
    [else (cons (f (car l)) (mapea f (cdr l)))]))

;8c - Filtra
(define (filtra p l)
  (cond
    [(empty? l) l]
    [(p (car l)) (cons (car l) (filtra p (cdr l)))]
    [(filtra p (cdr l))]))

;8d Primeros n elementos
(define (toma l n)
  (define (aux li i j)
    (cond
    [(zero? j) i]
    [else (aux (cdr li) (append i (list (car li))) (sub1 j))]))
  (aux l empty n))

;8e - quita
(define (quita l n)
  (define (aux1 li i j)
    (cond
      [(zero? j) i]
      [else (aux1 (toma li (add1 j)) (append i (list (last li))) (sub1 j))]))
  (aux1 l empty n))

;9 a
(letrec ([ mayor ( lambda ( n m )
                   ( if ( < n m) m
                        n))])
  (mayor 1834 1729))


;9 b

(letrec ([ sumas ( lambda ( n )
                   ( if ( < n 2) 1
                        (+ n ( sumas (- n 1)))))])
  (sumas 100))
