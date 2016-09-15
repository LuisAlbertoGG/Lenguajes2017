#lang plai
;1er ejercicio
; constructor de tipos 
(define-type Figure
  [triangulo (a number?) (b number?) (c number?)]
  [cuadrado (a number?)]
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
    [(rombo? fig) (* (rombo-a fig) 4)]
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
    [(cuadrado? fig) (* (cuadrado-a fig) (cuadrado-a fig))]
    [(rectangulo? fig) (* (rectangulo-a fig) (rectangulo-b fig))]
    [(rombo? fig) (/ (* (rombo-D fig) (rombo-d fig)) 2)]
    [(paralelogramo? fig) (* (paralelogramo-h fig) (paralelogramo-b fig))];supongo b la base
    [(circulo? fig) (* pi (expt (/ (circulo-D fig) 2) 2))]
    [(elipse? fig) (* pi (* (elipse-a fig) (elipse-b fig)))]))
;Un constructor (x) que representa una variable independiente x. Nótese que no recibe
;parámetros.
;---------------------------------------------------------------------------------------

;Ejercicio 2
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
    [(sum? fun) (sum (deriva (sum-f fun)) (deriva (sum-g fun)))]
    [(mul? fun) (sum (mul (deriva (mul-f fun)) (mul-g fun)) (mul (deriva (mul-g fun)) (mul-f fun)))]
    [(pot? fun) (mul (mul (pot-n fun) (pot (pot-b fun) (cte (- (funToNum (pot-n fun)) 1)))) (deriva (pot-b fun)))]))
    
(define (funToNum f)
  (cond
    [(cte? f) (cte-n f)]))
;-------------------------------------------------------------------------------------
;#Ejercicio3 Pilas y Colas :

(define (any? x) #t)

(define-type Nodo
  [vacio]
  [nodo (elemento any?)(siguiente Nodo?)])

(define-type Pila
  [pila (l Nodo?)]
  [mete-p (p pila?) (e any?)]
  [saca-p (p pila?)]
  [mira-p (p pila?)])

(define (calc-p pil)
  (cond
    [(pila? pil) (pila-l pil)]
    [(mete-p? pil) (pila (mete-pil (mete-p-p pil) (mete-p-e pil)))]
    [(saca-p? pil) (pila (saca-pil (saca-p-p pil)))]
    [(mira-p? pil) (cond
                     [(vacio?(nodo-siguiente (pila-l (mira-p-p pil)))) (nodo-elemento (pila-l (mira-p-p pil)))]
                     [else (calc-p (mira-p (pila (nodo-siguiente (pila-l (mira-p-p pil))))))])]))
(define (mete-pil pil e)
          (cond
              [(vacio? (nodo-siguiente (pila-l pil))) (nodo (nodo-elemento (pila-l pil)) (nodo e (vacio)))]
              [else (nodo (nodo-elemento (pila-l pil)) (mete-pil (pila(nodo-siguiente (pila-l pil))) e) )]
          ))

(define (saca-pil pil)
          (cond
              [(vacio? (nodo-siguiente (pila-l pil))) (vacio)]
              [else (nodo (nodo-elemento (pila-l pil)) (saca-pil (pila(nodo-siguiente (pila-l pil)))) )]
          ))

(define-type Cola
  [cola (l Nodo?)]
  [mete-c (c cola?) (e any?)]
  [saca-c (c cola?)]
  [mira-c (c cola?)])

(define (calc-c cc)
  (cond
    [(cola? cc) (cola-l cc)]
    [(mete-c? cc) (cola (mete-col (mete-c-c cc) (mete-c-e cc)))]
    [(saca-c? cc) (cola (saca-col (saca-c-c cc)))]
    [(mira-c? cc) (cond
                     [(vacio?(nodo-elemento (cola-l (mira-c-c cc)))) (vacio)]
                     [else (nodo-elemento (cola-l (mira-c-c cc)))])]))

(define (mete-col cc e)
          (nodo e (cola-l cc)))

(define (saca-col cc)
  (cond
    [(vacio? (cola-l cc)) (display "la pila ya es vacia")]
    [else (nodo-siguiente (cola-l cc))]))

(define (no-duplicados l)

  (if (l list?)
      (cond
    [(not (check-duplicates l)) #t]
    [else (remove-duplicates l)]
    ) 
      #f)
  )
;-------------------------------------------------------------------------------------
;Ejercicio 4
(define-type Conjunto 
  [conjunto (l list? ) ]
  [esvacio (l conjunto?)]
  [contiene (c conjunto?) (e number?)]
  [agrega (c1 conjunto?) (e number?)]
  [union (c1 conjunto?) (c2 conjunto?)]
  [interseccion (c1 conjunto?) (c2 conjunto?)]
  [diferencia (c1 conjunto?) (c2 conjunto?)]
  )


(define (calc-cjto c )
  (cond
  [(conjunto? c) ( remove-duplicates (conjunto-l c) )]
  [(esvacio? c) (empty? (conjunto-l(esvacio-l c)))]
  [(contiene? c) (contiene-elemento (conjunto-l (contiene-c c)) (contiene-e c))]
  [(agrega? c) (conjunto (remove-duplicates(append (conjunto-l (agrega-c1 c)) (cons  (agrega-e c) '()))))]
  [(union? c) (remove-duplicates (append (conjunto-l(union-c1 c)) (conjunto-l(union-c2 c))))]
  [(interseccion? c) (conjunto(saca (remove-duplicates(conjunto-l (interseccion-c1 c))) (remove-duplicates(conjunto-l (interseccion-c2 c)))))]
  [(diferencia? c) (remove* (conjunto-l(diferencia-c2 c)) (conjunto-l(diferencia-c1 c)))]
  
  )
)

(define (contiene-elemento l e)
  (cond
    [(not(member e l)) #f]
    [(list? (member e l)) #t]))


(define (recorre1 e l2)
    
  (cond
    [(empty? l2) '()]
    [(equal? (car l2) e) (cons e '())]
    [else (recorre1 e (cdr l2))]))

(define (saca l1 l2)
  (cond
    [(empty? l1) '()]
    [(empty? l2) '()]
    [else (append (recorre1 (car l1) l2) (saca (cdr l1) l2)) 
          ]))







