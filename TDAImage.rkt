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




;--------------------------------------------------Bitmap?----------------------------------------------------

;retorna 0 si no es bit y retorna 1 si es bit el numero revisado
(define (bit? bit)
  (cond
    [(or (= 0 bit) (= 1 bit)) 1]
    [else 0]))



;Llega hasta la posicion 3 (donde esta el bit) y llama a la funcion bit? para revisar si es un bit
(define (revisar_SubListas_bit lista contador)
  (cond
    [(= contador 2) (bit? (car lista))]
    [else (revisar_SubListas_bit (cdr lista) (+ contador 1))]))



;revisa la lista con sublistas
(define (revisarLista_bit lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_bit (cdr lista) (+ contador 1) (revisar_SubListas_bit (car lista) 0))))



;comprueba si una imagen es de formato bitmap. Retorna #t si es bit, de lo contrario retorna #f
(define (bitmap? image)
  (cond
    [(= (revisarLista_bit (acceder image 2) 0 2) 1) #t];Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
    [else #f]))

;(contar (acceder (image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1)) 2))

;(bitmap? (image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1)))
;-------------------------------------------------------------------------------------------------------------


;(revisarLista '((0 0 0 0) (0 0 0 0) (0 0 0 0)) 0 1) ;1
;(revisarLista '((0 0 1 0) (0 0 1 0) (0 0 1 0)) 0 1)
;(revisarLista '((0 0 0 0) (0 0 2 0) (0 0 0 0)) 0 1)
;(revisarLista '((0 0 0 0) (0 0 0 0) (0 0 2 0)) 0 1)
;(revisarLista '((0 0 3 0) (0 0 3 0) (0 0 0 0)) 0 1)
;(revisarLista '((0 0 0 0) (0 0 4 0) (0 0 4 0)) 0 1)
;(revisarLista '((0 0 5 0) (0 0 0 0) (0 0 5 0)) 0 1)
;(revisarLista '((0 0 0 0) (0 0 5 0) (0 0 0 0)) 0 1)
;(revisarLista '((0 0 6 0) (0 0 6 0) (0 0 6 0)) 0 1)







;--------------------------------------------------Pixmap?----------------------------------------------------

;retorna 0 si no es bit y retorna 1 si es bit el numero revisado
(define (pix? pix)
  (cond
    [(and (<= 0 pix) (>= 255 pix)) 1]
    [else 0]))



;Llega hasta la posicion 3 (donde esta el bit) y llama a la funcion bit? para revisar si es un bit
(define (revisar_SubListas_pix lista contador)
  (cond
    [(= contador 2) (pix? (car lista))]
    [else (revisar_SubListas_bit (cdr lista) (+ contador 1))]))



;revisa la lista con sublistas
(define (revisarLista_pix lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_pix (cdr lista) (+ contador 1) (revisar_SubListas_pix (car lista) 0))))



;comprueba si una imagen es de formato bitmap. Retorna #t si es bit, de lo contrario retorna #f
(define (pixmap? image)
  (cond
    [(= (revisarLista_pix (acceder image 2) 0 2) 1) #t];Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
    [else #f]))

;(contar (acceder (image 5 3 (pixrgb-d 0 0 1 0 0 9) (pixrgb-d 1 6 0 8 0 1)) 2))

(pixmap? (image 5 3 (pixrgb-d 0 0 9 9 1 0) (pixrgb-d 9 8 1 6 0 1)))
;-------------------------------------------------------------------------------------------------------------













































;----------------

;(recorrerListas(image 5 3 (pixbit-d 0 0 0 0) (pixbit-d 1 6 1 1))0)

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

