#lang racket
;Este archivo contiene funciones que son utilizadas por todos los TDA, por ende, no puede estar solo en un archivo

;------------------------------------------OTRAS FUNCIONES-------------------------------------------------

; Dominio: Lista (int) X posicion (int)
; Recorrido: Int
; Descripcion: Acceder a una posicion de una lista
; Tipo de recursion: Recursión de cola
(define (acceder lista posicion)
  (cond
    [(= 0 posicion) (car lista)]
    [else (acceder (cdr lista) (- posicion 1))]))


; Dominio: lista (int)
; Recorrido: int
; Descripcion: Contar los elementos dentro de una lista
; Tipo de recursion: Recursión natural
(define (contar lista)
  (cond
    [(null? lista) 0]
    [else (+ 1 (contar (cdr lista)))]))


; Dominio: lista (list)
; Recorrido: int
; Descripcion: Multiplica el largo por el ancho de una imagen
; Tipo de recursion: No se utiliza recursión
(define (tamanio lista)
  (* (acceder lista 0) (acceder lista 1)))


; Dominio: lista (list) X elemento (int)
; Recorrido: list
; Descripcion: Elimina un determinado elemento dentro de una lista
; Tipo de recursion: Recursión natural
(define (eliminar lista elemento)
  (cond
    [(null? lista) '()]
    [(eq? (car lista) elemento) (eliminar (cdr lista) -1)];Se coloca el -1 para que la función deje de buscar el valor, ya que borra todos los elementos que sean iguales al ingresado
    [else  (cons (car lista) (eliminar (cdr lista) elemento))]))


; Dominio: a (str) X b (str) X c (str) X d (str) X e (str) X f (str) X g (str) X h (str) X i (str) X j (str) X k (str) X l (str) X m (str) X n (str) X o (str) X p (str)
; Recorrido: List (str)
; Descripcion: Crea uan lista de elementos. Es utilizada para crear la lista de valores Hexadecimal
; Tipo de recursion: No se utiliza recursión
(define (lista_Hexadecimal a b c d e f g h i j k l m n o p)
  (list  a b c d e f g h i j k l m n o p))

;Se define la lista Hexadecimal
(define Hexadecimal (lista_Hexadecimal  "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))
































(provide (all-defined-out))