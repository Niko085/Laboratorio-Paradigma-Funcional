#lang racket
; Se requiere el uso del TDA image, TDA bitmap, TDA hexmap y TDA pixmap
(require "otras_funciones.rkt")

; Dominio: posicionx (int) X posiciony (int) X bit([0|1]) X Profundidad (int)
; Recorrido: Int
; Descripcion: Crea un pixel de tipo bitmap
; Tipo de recursion: No se utiliza recursion
(define (pixbit-d posicionX posicionY bit profundidad)
  (list posicionX posicionY bit profundidad ))



;--------------------------------------------------Bitmap?----------------------------------------------------
; Dominio: pix (string)
; Recorrido: int
; Descripcion: Retorna 0 si no es bit y retorna 1 si es bit el numero revisado
; Tipo de recursion: No se utiliza recursión
(define (bit? bit)
  (cond
    [(or (= 0 bit) (= 1 bit)) 1]
    [else 0]))


; Dominio: lista (list) X contador (int)
; Recorrido: int
; Descripcion: Llega hasta la posicion 3 (donde esta el bit) y llama a la funcion bit? para revisar si es un bit
; Tipo de recursion: Recursión de cola
(define (revisar_SubListas_bit lista contador)
  (cond
    [(= contador 2) (bit? (car lista))]
    [else (revisar_SubListas_bit (cdr lista) (+ contador 1))]))


; Dominio: lista (list) X contador (int) X revisado (int)
; Recorrido: int
; Descripcion: Revisa la lista con sublistas
; Tipo de recursion: Recursión de cola
(define (revisarLista_bit lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_bit (cdr lista) (+ contador 1) (revisar_SubListas_bit (car lista) 0))))


; Dominio: image (list)
; Recorrido: Boolean
; Descripcion: Comprueba si una imagen es de formato bitmap. Retorna #t si es bit, de lo contrario retorna #f
; Tipo de recursion: Contar utiliza recursión natural y acceder utiliza recursión de cola
(define (bitmap? lista)
  (cond
    [(not (= (contar (acceder (acceder lista 2) 0)) 4)) #f]
    [(= (revisarLista_bit (acceder lista 2) 0 2) 1) #t]
    [else #f]));Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
    
;(image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1))
;(contar (acceder (image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1)) 2))

;(bitmap? (image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1)))
;-------------------------------------------------------------------------------------------------------------


















;----------------------------------------------------------------------------OTRAS FUNCIONES------------------------------------------------------------------



;Esta función es utilizada para hacer uso del TDA en el archivo main.rkt
(provide (all-defined-out))