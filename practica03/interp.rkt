#lang plai

(require "FWAE.rkt")
(require "parser.rkt")

;; Análisis semántico del lenguaje. El analizador semántico se encarga de dar
;; un significado al árbol de sintaxis abstracta.


;; Función que implementa el algoritmo de sustitución.
;; subst: FWAE symbol FWAE -> FWAE
(define (subst expr sub-id val)
   (match expr
      #| Para números simplemente devolvemos su valor pues no hay nada que
         sustituir. |#
      [(num n) expr]
      #| Para operaciones binarias regresamos una nueva operación binaria con
         la sustitución del lado izquierdo y el lado derecho. |#
      [(binop f l r) (binop f (subst l sub-id val) (subst r sub-id val))]
      #| Para with tenemos que considerar dos casos |#
      [(with temp bound-body) ; MODIFICAR ESTE CASO bound-id named-expr=temp
         (if (symbol=? (binding-name temp) sub-id)
            #| Si el identificador de la expresión with es igual al del 
               valor a sustituir, simplemente sustituimos en la expresión
               asociada al identificador, pues el alcance del cuerpo del 
               with hace referencia al identificador y no podemos cambiar 
               dicho valor. |#
            (with 
               (binding-name temp) 
               (subst (binding-value temp) sub-id val)
               bound-body)
            #| Si el identificador de la expresión with es distinto al del
               valor a sustituir, sustituimos la expresión asociada al 
               indentificador y al cuerpo, pues esto no afecta el 
               alcance. |#
            (with
               (binding-name temp)
               (subst (binding-value temp) sub-id val)
               (subst bound-body sub-id val)))]
      #| Para fun tenemos que considerar dos casos. |#
      [(fun bound-id bound-body) ; MODIFICAR ESTE CASO
         (if (symbol=? bound-id sub-id)
            #| Si el parámetro de la función es igual al del valor a 
               sustituir no hacemos nada para evitar modificar el 
               alcance. |#
            (fun
               bound-id
               bound-body)
            #| Si el parámetro de la función es distinto al del valor a
               sustituir sólo sustituimos en el cuerpo. |#
            (fun
               bound-id
               (subst bound-body)))]
      #| Para la aplicación de funciones simplemente sustituimos 
         recursivamente el valor en la funció y los argumentos. |#
      [(app fun-expr arg-expr)
         (app 
            (subst fun-expr sub-id val)
            (subst arg-expr sub-id val))]
      #| Para identificadores, sustituimos la expresión si es igual al valor
         a sustituir, en caso contrario, no hacemos nada. |#
      [(id v)
         (if (symbol=? v sub-id)
            val
            expr)]))

;; Función encargada de interpretar el árbol de sintaxis abstracta generador por
;; el parser.
;; interp: AE -> any
(define (interp exp)
   (match exp
      #| Un número se evalúa a un número |#
      [(num n) n]
      #| Aplicamos la función f a la interpretación del lado izquierdo y 
         derecho. |#
      [(binop f l r) (f (interp l) (interp r))]
      #| Interpretamos el cuerpo con la sustitución del identificador 
         correspondiente. |#
      [(with temp bound-body) ; MODIFICAR ESTE CASO
         (interp (subst bound-body (binding-name temp) (num (interp (binding-value temp)))))]
      #| En esta versión del intérprete, no regresamos ningún resultado al
         interpretar una función. Una función es una función y punto. |#
      [(fun bound-id bound-body) "#<function>"]
      #| Para aplicar la función debemos sustituir el parámetro de la función
         por por el argumento en el cuerpo de la función y luego 
         interpretarla. |#
      [(app fun-expr arg-expr) ; MODIFICAR ESTE CASO
         (interp 
            (subst (fun-body fun-expr) ; obtenemos el cuerpo de la función
                  (fun-param fun-expr) ; obtenemos el parámetro de la función
                  (num (interp arg-expr))))] ; converimos a número el argumento
      #| El identificador no tiene valor asociado, por lo que decimos ques es
         una variable libre. |#
      [(id v) "free identifier"]))



;; Función encargada de ejecutar el interprete para que el usuario interactúe
;; con el lenguaje. Para diferenciar el prompt de Racket del nuestro, usamos
;; "(λ)". Aprovechamos los aspectos imperativos del lenguaje.
(define (ejecuta)
   (begin
      (display "(λ) ")  ; imprimimos el prompt
      (define x (read)) ; pedimos la expresión
      (if (equal? x '{exit}) ; si nos piden salir del intérprete
         (display "") ; no hacemos nada
         (begin ; en otro caso realizamos la interpretación
            (display (interp (parse x))) ; interpretamos el árbol de sintaxis abstracta
            (display "\n") ; da un salto de línea
            (ejecuta))))) ; repite el procesos hasta que se lea {exit}.

;; Llamada a función encargada de iniciar la ejecución del interprete
(display "Bienvenido al Lenguaje Tlacuache v1.0.\n") ; bienvenida al usuario
(ejecuta) ; llama a ejecución al intérprete

