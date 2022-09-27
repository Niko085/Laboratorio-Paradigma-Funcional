#lang racket
; Se requiere el uso del TDA image, TDA bitmap, TDA hexmap y TDA pixmap
(require "otras_funciones.rkt")

; Dominio: posicionx (int) X posiciony (int) X r (int) X g (int) X b (int) X Profundidad (int)
; Recorrido: Int
; Descripcion: Crea un pixel de tipo pixmap
; Tipo de recursion: No se utiliza recursion
(define (pixrgb-d posicionX posicionY red green blue profundidad)
  (list posicionX posicionY red green blue profundidad ))

;--------------------------------------------------Pixmap?----------------------------------------------------
; Dominio: pix (string)
; Recorrido: int
; Descripcion: Retorna 0 si no es pix, y retorna 1 en caso de si serlo
; Tipo de recursion: No se utiliza recursión
(define (pix? pix)
  (cond
    [(and (<= 0 pix) (>= 255 pix)) 1]
    [else 0]))


; Dominio: lista (list) X contador (int)
; Recorrido: int
; Descripcion: LLama a la función pix para comprobar si una función es pix
; Tipo de recursion: acceder utiliza recursión de cola
(define (revisar_SubListas_pix lista contador)
  (cond
    [(and (= 1 (pix? (acceder lista 2))) (= 1 (pix? (acceder lista 3))) (= 1 (pix? (acceder lista 4)))) 1]
    [else 0]))


; Dominio: lista (list) X contador (int) X revisado (int)
; Recorrido: int
; Descripcion: Revisa la lista con sublistas ej: '((1 2 3 4) (5 6 7 8))
; Tipo de recursion: Recursión de cola
(define (revisarLista_pix lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_pix (cdr lista) (+ contador 1) (revisar_SubListas_pix (car lista) 0))))


; Dominio: image (list)
; Recorrido: Boolean
; Descripcion: Comprueba si una imagen es de formato pixmap. Retorna #t si es pix, de lo contrario retorna #f
; Tipo de recursion: Contar utiliza recursión natural y acceder utiliza recursión de cola
(define (pixmap? lista)
  (cond
    [(not (= (contar (acceder (acceder lista 2) 0)) 6)) #f]
    [(= (revisarLista_pix (acceder lista 2) 0 2) 1) #t]
    [else #f]));Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
   

;(contar (acceder (image 5 3 (pixrgb-d 0 0 1 0 0 9) (pixrgb-d 1 6 0 8 0 1)) 2))

;(pixmap? (image 5 3 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))


;(pix? (acceder '(1 2 3 4) 0))
;(pix? (acceder '(1 2 3 4) 1))
;(pix? (acceder '(1 2 3 4) 2))
;(pix? (acceder '(1 2 3 256) 3))
;-------------------------------------------------------------------------------------------------------------


















;----------------------------------------------------------------------------OTRAS FUNCIONES------------------------------------------------------------------



;Esta función es utilizada para hacer uso del TDA en el archivo main.rkt
(provide (all-defined-out))