#lang racket
; Se requiere el uso del TDA image, TDA bitmap, TDA hexmap y TDA pixmap
(require "otras_funciones.rkt")

; Dominio: posicionx (int) X posiciony (int) X hex(String) X Profundidad (int)
; Recorrido: Int
; Descripcion: Crea un pixel de tipo hexmap
; Tipo de recursion: No se utiliza recursion
(define (pixhex-d posicionX posicionY color profundidad)
  (list posicionX posicionY color profundidad ))


;--------------------------------------------------Hexmap?----------------------------------------------------
; Dominio: hex (string)
; Recorrido: int
; Descripcion: Retorna 0 si no es hex, y retorna 1 en caso de si serlo
; Tipo de recursion: No se utiliza recursión
(define (hex? hex)
  (cond
    [(string? hex) 1]
    [else 0]))


; Dominio: lista (list) X contador (int)
; Recorrido: int
; Descripcion: LLama a la función hex para comprobar si una función es hex
; Tipo de recursion: acceder utiliza recursión de cola
(define (revisar_SubListas_hex lista contador)
  (cond
    [(and (= 1 (hex? (acceder lista 2)))) 1]
    [else 0]))


; Dominio: lista (list) X contador (int) X revisado (int)
; Recorrido: int
; Descripcion: Revisa la lista con sublistas ej: '((1 2 3 4) (5 6 7 8))
; Tipo de recursion: Recursión de cola
(define (revisarLista_hex lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_hex (cdr lista) (+ contador 1) (revisar_SubListas_hex (car lista) 0))))


; Dominio: image (list)
; Recorrido: Boolean
; Descripcion: Comprueba si una imagen es de formato hexmap. Retorna #t si es hex, de lo contrario retorna #f
; Tipo de recursion: Contar utiliza recursión natural y acceder utiliza recursión de cola
(define (hexmap? lista)
  (cond
    [(not (= (contar (acceder (acceder lista 2) 0)) 4)) #f]
    [(= (revisarLista_hex (acceder lista 2) 0 2) 1) #t]
    [else #f]));Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
;-------------------------------------------------------------------------------------------------------------------
















;----------------------------------------------------------------------------OTRAS FUNCIONES------------------------------------------------------------------



;Esta función es utilizada para hacer uso del TDA en el archivo main.rkt
(provide (all-defined-out))