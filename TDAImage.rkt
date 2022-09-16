#lang racket

(require "bitmap.rkt")
(require "hexmap.rkt")
(require "pixmap.rkt")

;-----------------------------------TDA Image------------------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------------

; Este TDA representa imagenes con bitmaps, hexmaps o pixmaps, además, incluye información de profundidad
; en cada pixel

;-----------------------------------CONSTRUCTORES---------------------------------------------------------------------

;bitmap -> pixbit-d

;pixmap -> pixrgb-d

;hexmap -> pixhex-d

(define tamanio (lambda (n1 n2)
                  (* n1 n2)
  ))




; Dominio: Ancho (int) X alto (int) X [pixbit-d* | pixrgb-d* | pixhex]
; Recorrido: Image
; Descripcion: Crea una imagen en base a pixeles 
; Tipo de recursion: No utiliza recursión
(define (image ancho alto . pixeles)
  (list ancho alto pixeles))


;

; Dominio: Lista (int) X posicion (int)
; Recorrido: Int
; Descripcion: Acceder a una posicion de una lista
; Tipo de recursion: Recursión de cola
(define (acceder lista posicion)
  (cond
    [(= 0 posicion) (car lista)]
    [else (acceder (cdr lista) (- posicion 1))]))




;Contar elementos de una lista
(define (contar lista)
  (cond
    [(null? lista) 0]
    [else (+ 1 (contar (cdr lista)))]))



;-----------------PRUEBAS---------------
(image 1 2 (pixbit-d 1 0 1 0) (pixbit-d 1 0 1 0) (pixbit-d 1 0 1 0) (pixbit-d 1 0 1 0))

(contar (image 1 2 (pixbit-d 1 0 1 0) (pixbit-d 1 0 1 0) (pixbit-d 1 0 1 0) (pixbit-d 1 0 1 0)))



(image 5 3 (pixbit-d 0 0 0 0) (pixbit-d 1 6 1 1))
(acceder (acceder (acceder (image 1 2 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1)) 2) 1) 2)





(define (bitmap? image)
  (cond
    [(< 1 3) (acceder (acceder image 2) 1)]))



(bitmap? (image 5 3 (pixbit-d 0 0 0 0) (pixbit-d 1 6 1 1)))




print "ugu"

;(define (contar lista)
 ; (cond
  ;  [(null? lista) 0]
   ; [else (+ 1 (contar (cdr lista)))]))

;Cuenta los elementos de una lista dentro de la lista formada despues del punto de la funcion image
(define (recorrerListas lista acum)
  (cond
    [(= acum 2) (contar (acceder (car lista) 0))]
    [else (recorrerListas (cdr lista) (+ acum 1))]))

(recorrerListas(image 5 3 (pixbit-d 0 0 0 0) (pixbit-d 1 6 1 1))0)

;(define imagen a)
; Dominio: 
; Recorrido: 
; Descripcion: Crea una imagen de 2 x 2 del tipo pixmap
; Tipo de recursion: No se utiliza recursion
(define Img1 (image 2 2
                   (pixrgb-d 0 0 255 0 0 10) ;  FF0000 toma 255 0 0
                   (pixrgb-d 0 1 0 255 0 20) ;  00FF00 toma 0 255 0
                   (pixrgb-d 1 0 0 0 255 10) ;  0000FF toma 0 0 255
                   (pixrgb-d 1 1 255 255 255 1) ;FFFFFF toma 255 255 255
  ))

;(image 5 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))




























; Dominio: 
; Recorrido: 
; Descripcion:
; Tipo de recursion: No se utiliza recursion


; Dominio: 
; Recorrido: 
; Descripcion:
; Tipo de recursion: 


;-----------------------------------OTRAS FUNCIONES-------------------------------------------------------------------





;Esta función es utilizada para hacer uso del TDA en el archivo main.rkt
(provide (all-defined-out))

