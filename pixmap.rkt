#lang racket
;Función requerida por el TDA Image

; Dominio: posicionx (int) X posiciony (int) X r (int) X g (int) X b (int) X Profundidad (int)
; Recorrido: Int
; Descripcion: Crea un pixel de tipo pixmap
; Tipo de recursion: No se utiliza recursion
(define (pixrgb-d posicionX posicionY red green blue profundidad)
  (list posicionX posicionY red green blue profundidad ))






















(provide (all-defined-out)) 