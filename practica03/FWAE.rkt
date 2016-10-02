#lang plai
;; Tipo de de tado Bindig qeu alamcen sibolo y valor tipo FWAE
(define-type Binding
  [binding (name symbol?) (value FWAE?)])
;; Tipo de dato abstracto FWAE para representar el árbol de sintaxis abstracta.
;; El lenguaje FWAE reconoce expresiones numéricas, operaciones binarias,
;; asignaciones locales, identificadores, funciones anónimas (lambdas) y 
;; aplicación de funciones.
(define-type FWAE
  [num (n number?)]
  [binop (f symbol?) (l FWAE?) (r FWAE?)]
  [with (bindings (listof Binding?))(body FWAE?)]
  [id (name symbol?)]
  [fun (param (listof symbol?)) (body FWAE?)]
  [app (fun-expr FWAE?) (args (listof FWAE?))])
