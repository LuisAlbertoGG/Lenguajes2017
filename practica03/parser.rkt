#lang plai

(require "FWAE.rkt")

;; Análisis sintáctico del lenguaje. En analizador sintáctico se encarga de
;; construir el árbol de sintaxis abstracta.

;; Función elige auxiliar.
;; Permite elegir el operador correspondiente en Racket para las operaciones
;; binarias.
;; elige: symbol -> procedure
(define (elige s)
   (case s
      [(+) +]
      [(-) -]
      [(*) *]
      [(/) /]
      [(%) modulo]
      [(max) max]
      [(min) min]
      [(pow) expt]))

;; Analizador sintáctico para FWAE.
;; Dada una expresión en sintaxis concreta, regresa el árbol de sintaxis
;; abstractca correspondiente. Es decir, construye expresiones del tipo de
;; dato abstracto definido anteriormente.
;; parse: symbol -> FWAE
(define (parse sexp)
   (cond
      [(symbol? sexp) (id sexp)] ; para identificadores
      [(number? sexp) (num sexp)] ; para números
      [(list? sexp)
         (case (car sexp)
            [(+ - * - / % max min pow) ; para operaciones binarias
               (binop
                  (elige (car sexp))
                  (parse (cadr sexp))
                  (parse (caddr sexp)))]
            [(with) ; para asignaciones locales MODIFICAR ESTE CASO
               (map (lambda (anom) (with
                  (caadr anom)
                  (parse (cadadr anom))
                  (parse (caddr anom)))) sexp)]
            [(fun) ; para lambdas MODIFICAR ESTE CASO
               (map (lambda (anom) (fun
                  (caadr anom)
                  (parse (caddr anom)))) sexp)]
            [else ; para aplicación de funciones
               (app
                  (parse (car sexp))
                  (parse (cadr sexp)))])]))