#lang plai
;; constructor de tipos 
(define-type Figure
  [triangulo (a number?) (b number?) (c number?)]
  [cuadrado (a number?)] ; constructor 
  [rectangulo (a number?) (b number?)]
  [rombo (a number?) (D number?) (d number?)]
  [paralelogramo (a number?) (b number?) (h number?)]
  [circulo (D number?)]
  [elipse (a number?) (b number?)])

; saca el perimetro 
(define (perimetro fig)
  (cond
    [(triangulo? fig) (+ (+ (triangulo-a fig) (triangulo-b fig)) (triangulo-c fig))]
    [(cuadrado? fig) (* (cuadrado-a fig) 4)]
    [(rectangulo? fig) (* (+ (rectangulo-a fig) (rectangulo-b fig)) 2)]
    [(rombo? fig) (* (rombo-a) 4)]
    [(paralelogramo? fig) (* (+ (paralelogramo-a fig) (paralelogramo-b fig)) 2)]
    [(circulo? fig) (* pi (circulo-D fig))]
    [(elipse? fig) (* (* 2 pi) (sqrt (/ (+ (* (elipse-a fig) (elipse-a fig)) (* (elipse-b fig) (elipse-b fig))) 2)))]))

;area de el la figura

(define (area fig)
  (cond
    [(triangulo? fig) (sqrt (* (* (* (/ (+ (triangulo-a fig) (+ (triangulo-b fig) (triangulo-c fig))) 2)
                               (- (/ (+ (triangulo-a fig) (+ (triangulo-b fig) (triangulo-c fig))) 2) (triangulo-a fig)))
                            (- (/ (+ (triangulo-a fig) (+ (triangulo-b fig) (triangulo-c fig))) 2) (triangulo-b fig)))
                         (- (/ (+ (triangulo-a fig) (+ (triangulo-b fig) (triangulo-c fig))) 2) (triangulo-c fig))))]
    [(cuadrado? fig) (* (cuadrado-a fig) (cuadrado-a))]
    [(rectangulo? fig) (* (rectangulo-a fig) (rectangulo-b fig))]
    [(rombo? fig) (/ (* (rombo-D fig) (rombo-d fig)) 2)]
    [(paralelogramo? fig) (* (paralelogramo-h fig) (paralelogramo-b fig))];supongo b la base
    [(circulo? fig) (* pi (expt (/ (circulo-D fig) 2) 2))]
    [(elipse? fig) (* pi (* (elipse-a fig) (elipse-b fig)))]))
;Un constructor (x) que representa una variable independiente x. Nótese que no recibe
;parámetros.

(define-type Funcion
  [x]
  [cte (n number?)]
  [sum (f Funcion?) (g Funcion?)]
  [mul (f Funcion?) (g Funcion?)]
  [pot (b Funcion?) (n Funcion?)])

(define (Funcion->string fun)
  (cond
    [(x? fun) "x"]
    [(cte? fun) (number->string(cte-n fun))] ;Un constructor (cte n) para representar constantes como 2 o 1729, n es un número entero.
    [(sum? fun) (string-append "(" (Funcion->string (sum-f fun)) "+" (Funcion->string (sum-g fun)) ")")];Un constructor (sum f g) que representa una suma de funciones como x + 2, dónde f y g son funciones.
    [(mul? fun)(string-append "(" (Funcion->string (mul-f fun)) "*" (Funcion->string (mul-g fun)) ")")]
    [(pot? fun) (string-append "(" (Funcion->string (pot-b fun)) "^" (Funcion->string (pot-n fun)) ")")]))

(define (evalua fun v)
  (cond
    [(x? fun) (cte v)]
    [(cte? fun) (cte (cte-n fun))]
    [(sum? fun) (append(sum (evalua (sum-f fun) v) (evalua (sum-g fun) v)))]
    [(mul? fun) (append(mul (evalua (mul-f fun) v) (evalua (mul-g fun) v)))]
    [(pot? fun) (append(pot (evalua (pot-b fun) v) (evalua (pot-n fun) v)))]))

(define (deriva fun)
  (cond
    [(x? fun) (cte 1)]
    [(cte? fun) (cte 0)]
    [(sum? fun) (sum (deriva (sum-f fun)) (deriva(sum-g fun)))]
    [(mul? fun) (mul (pot-n fun) (pot (pot-b fun) (- (pot-n fun) 1)))]))

;(define (derivada f)
;   (cond
;    [(cte? f) (cte 0)]
;    [(fun? f) (cond
;                   [()])]))

; car primero
; cdr el ultimo
;ejecccio #3 Pilas y Colas :

;(define-type Nodo
;  [vacio]
;  [nodo (any  elemento )(siguiente Nodo?)])

;(define elemento
;  [pila (n Nodo?)]
;  [mete-p (e Nodo?)(p Nodo?)]
;  [saca-p (p Nodo?)]
;  [mira-p (p Nodo?)]
  
 ; )
;(define (pila n)
 ; (cond
 ;   [(n? Nodo)]
 ;   [(me? )]
;    
   ; )
  ;)

 
;constructor pila n :


;mete-p e p :

;(saca-p p) : elimina un elemento en la pila , elimina al final 

;contructor de (mira-p) p :








