#lang racket
;Función requerida por el TDA Image

; Dominio: posicionx (int) X posiciony (int) X bit([0|1]) X Profundidad (int)
; Recorrido: Int
; Descripcion: Crea un pixel de tipo bitmap
; Tipo de recursion: No se utiliza recursion
(define (pixbit-d posicionX posicionY bit profundidad)
  (list posicionX posicionY bit profundidad ))


(define (acceder lista posicion)
  (cond
    [(= 0 posicion) (car lista)]
    [else (acceder (cdr lista) (- posicion 1))]))
























(provide (all-defined-out))