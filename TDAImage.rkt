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
    [(not (= (contar (acceder (acceder image 2) 0)) 4)) #f]
    [(= (revisarLista_bit (acceder image 2) 0 2) 1) #t]
    [else #f]));Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
    
;(image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1))
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

;retorna 0 si no es pix, y retorna 1 en caso de si serlo
(define (pix? pix)
  (cond
    [(and (<= 0 pix) (>= 255 pix)) 1]
    [else 0]))



;Revisa las posiciones 2, 3 y 4 de cada lista comprobando si es un pixel válido (Esas son las posiciones porque el contador parte de 0)
(define (revisar_SubListas_pix lista contador)
  (cond
    [(and (= 1 (pix? (acceder lista 2))) (= 1 (pix? (acceder lista 3))) (= 1 (pix? (acceder lista 4)))) 1]
    [else 0]))



;revisa la lista con sublistas ej: '((1 2 3 4) (5 6 7 8))
(define (revisarLista_pix lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_pix (cdr lista) (+ contador 1) (revisar_SubListas_pix (car lista) 0))))



;comprueba si una imagen es de formato pixmap. Retorna #t si es pix, de lo contrario retorna #f
(define (pixmap? image)
  (cond
    [(not (= (contar (acceder (acceder image 2) 0)) 6)) #f]
    [(= (revisarLista_pix (acceder image 2) 0 2) 1) #t]
    [else #f]));Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
   

;(contar (acceder (image 5 3 (pixrgb-d 0 0 1 0 0 9) (pixrgb-d 1 6 0 8 0 1)) 2))

;(pixmap? (image 5 3 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))



;(pix? (acceder '(1 2 3 4) 0))
;(pix? (acceder '(1 2 3 4) 1))
;(pix? (acceder '(1 2 3 4) 2))
;(pix? (acceder '(1 2 3 256) 3))
;-------------------------------------------------------------------------------------------------------------


;--------------------------------------------------Hexmap?----------------------------------------------------

;retorna 0 si no es hex, y retorna 1 en caso de si serlo
(define (hex? hex)
  (cond
    [(string? hex) 1]
    [else 0]))



;Revisa las posiciones 2, 3 y 4 de cada lista comprobando si es un pixel válido (Esas son las posiciones porque el contador parte de 0)
(define (revisar_SubListas_hex lista contador)
  (cond
    [(and (= 1 (hex? (acceder lista 2)))) 1]
    [else 0]))


;revisa la lista con sublistas ej: '((1 2 3 4) (5 6 7 8))
(define (revisarLista_hex lista contador revisado) ;El valor inicial de revisado debe ser mayor que 0, en este caso se usa 2
  (if (or (null? lista) (= revisado 0)) 
      revisado
      (revisarLista_hex (cdr lista) (+ contador 1) (revisar_SubListas_hex (car lista) 0))))


;comprueba si una imagen es de formato hexmap. Retorna #t si es hex, de lo contrario retorna #f
(define (hexmap? image)
  (cond
    [(not (= (contar (acceder (acceder image 2) 0)) 4)) #f]
    [(= (revisarLista_hex (acceder image 2) 0 2) 1) #t]
    [else #f]));Se accede a la posicion 2 del la lista image porque corresponde a la lista formada después del punto por el constructor image
;-------------------------------------------------------------------------------------------------------------------



;----------------------------------------------------Compressed?----------------------------------------------------

;Multiplica el largo por el ancho de una imagen
(define (tamanio lista )
  (* (acceder lista 0) (acceder lista 1)))


;(tamanio (image 5 3 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))


;Comprueba que la cantidad de pixeles no haya sido comprimida. Retorna #f si no está comprimida, retorna #t si esta comprimida y da aviso en caso de haber ingresado mas pixeles de los que debería
(define (compressed? image)
  (cond
    [(< (tamanio image) (contar (acceder image 2))) print "Hay mas pixeles que el tamanio declarado"]
    [(= (tamanio image) (contar (acceder image 2))) #f];no comprimida
    [(> (- (tamanio image) 1) (contar (acceder image 2))) print "No se puede volver a comprimir"]
    [(= (- (tamanio image) 1) (contar (acceder image 2))) #t]));compimida

;pruebas
;(compressed? (image 5 3 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))

;(compressed? (image 1 2 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))

;-------------------------------------------------------------------------------------------------------------------



;-------------------------------------------------------flipH-------------------------------------------------------
;Se generan todas las coordenadas posibles dependiendo del ancho y del alto
(define (coordenadas ancho alto cont1 cont2 lista)
  (cond
    [(= alto cont1) (reverse lista)]
    [(= ancho cont2) (coordenadas ancho alto (+ 1 cont1) 0 lista)]
    ;[else (coordenadas ancho alto cont1 (+ 1 cont2) (cons (list cont1 cont2) lista))]))
    [else (coordenadas ancho alto cont1 (+ 1 cont2) (cons cont2 (cons cont1 lista)))]))

;Elimina un determinado elemento dentro de una lista
(define (eliminar lista elemento)
  (if (null? lista)
      '()
      (if (= (car lista) elemento)
          (eliminar (cdr lista) -1);Se coloca el -1 para que la función deje de buscar el valor, ya que borra todos los elementos que sean igual al ingresado
          (cons (car lista) (eliminar (cdr lista) elemento)))))


;elimina las coordenadas de un pixel
(define (eliminar_coordenadas lista contador)
  (cond
    [(< contador 1) (eliminar_coordenadas (eliminar lista (acceder lista 0)) (+ contador 1))]
    [else lista]))

;Cambia las coordenadas de un pixel
(define (eliminar_e_insertar lista_coordenadas pixel contador posicion)
  (cond
    [(< contador 2) (eliminar_e_insertar lista_coordenadas (eliminar_coordenadas pixel 0) (+ contador 1) posicion)]
    [else (cons (acceder lista_coordenadas posicion)(cons (acceder lista_coordenadas (+ posicion 1)) pixel))]))


;(eliminar_coordenadas '(0 1 2 3 4) 0)

;Llama a la función para eliminar las coordenadas anteriores y agregar las nuevas
(define (entregar_coordenadas lista_coordenadas lista_pixeles cantidad contador contador2 nueva_lista)
  (cond
    [(and (not (= contador cantidad)) (<= contador2 (+ cantidad cantidad))) (entregar_coordenadas lista_coordenadas lista_pixeles cantidad (+ contador 1) (+ contador2 2) (cons (eliminar_e_insertar lista_coordenadas (acceder lista_pixeles contador) 0 contador2) nueva_lista))]
    [else (reverse nueva_lista)]))



;invierte un tramo de la lista y
(define (invertir lista contador salida)
  (cond
    [(= contador salida) lista]
    [else (append (invertir (cdr lista) (- contador 1) salida) (list (car lista)) )]))


;LLama a la funcion invertir listas 
(define (llamar_inversion lista cantidad contador ancho)
 (cond
   [(= contador 0) lista]
   [else (llamar_inversion (invertir lista cantidad (- cantidad ancho)) (- cantidad ancho) (- contador ancho) ancho)]))

;invierte una imagen horizontalmente
(define (flipH lista)
  (cond
    [(not (null? lista)) (entregar_coordenadas (coordenadas (acceder lista 0) (acceder lista 1) 0 0 null) (llamar_inversion (acceder lista 2) (contar (acceder lista 2)) (tamanio lista) (acceder lista 0)) (contar (acceder lista 2)) 0 0 null)]))

;(flipH (image 3 3 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5) (pixbit-d 6 6 6 6) (pixbit-d 7 7 7 7) (pixbit-d 8 8 8 8)))
;1 0 3 2

;(flipH (image 2 3 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5)))

;-------------------------------------------------------------------------------------------------------------------


;-------------------------------------------------------flipV-------------------------------------------------------


;Forma sub-listas del tamaño que el ancho de la imagen
(define (sub-listas lista ancho contador lista_aux lista_aux2)
  (cond
    [(null? lista) (reverse (cons (reverse lista_aux) lista_aux2))]
    [(= ancho contador) (sub-listas (cdr lista) ancho 1 (cons (car lista) null)(cons (reverse lista_aux) lista_aux2))]
    [else (sub-listas (cdr lista) ancho (+ contador 1) (cons (car lista) lista_aux) lista_aux2)]))

(sub-listas '('(0 0 0 0) '(1 1 1 1) '(2 2 2 2) '(3 3 3 3) '(4 4 4 4) '(5 5 5 5) '(6 6 6 6) '(7 7 7 7) '(8 8 8 8) '(9 9 9 9) '(10 10 10 10) '(11 11 11 11)) 4 0 null null)

  

;invierte una imagen verticalmente
(define (flipV lista)
  (cond                       
    [(not (null? lista)) (llamar_inversion (acceder lista 2) (contar (acceder lista 2)) (tamanio lista (acceder lista 0)))]))

;(flipV (image 3 2 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5)))
;3 4 5 0 1 2
;-------------------------------------------------------------------------------------------------------------------


(agrupa 4 0 '('(0 0 0 0) '(1 1 1 1) '(2 2 2 2) '(3 3 3 3) '(4 4 4 4) '(5 5 5 5) '(6 6 6 6) '(7 7 7 7) '(8 8 8 8) '(9 9 9 9) '(10 10 10 10) '(11 11 11 11)) null null)




;(list(cons '(1 2 3)(cons '(4 5 6) (cons '(7 8 9) null))))























;(eliminar_coordenadas '(1 2 3 4 5 6 7 8 9) 0)




;(eliminar '(1 2 3 4) 2)
;(equal? '(1 2 3 45) '(1 2 3 45))
;(equal? '(1 2 3 5) '(1 2 3 45))
(define (lista x y z)
  (list x y z))

;(elimina '((0 0 0 0) (1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4)) '(2 2 2 2) '((0 0 0 0) (1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4)) 2)
;(eliminar (lista 1 2 3) (acceder(lista 1 2 3) 0))










;------------------------------------------------------------------------------PRUEBAS--------------------------------------------------------------------

;(recorrerListas(image 5 3 (pixbit-d 0 0 0 0) (pixbit-d 1 6 1 1))0)

;(define imagen a)
; Dominio: 
; Recorrido: 
; Descripcion: Crea una imagen de 2 x 2 del tipo pixmap
; Tipo de recursion: No se utiliza recursion
(define img1 (image 2 2
                   (pixrgb-d 0 0 0 255 0 10) ;  FF0000 toma 255 0 0      ;mas elementos de los que debería
                   (pixrgb-d 0 1 0 255 0 20) ;  00FF00 toma 0 255 0
                   (pixrgb-d 1 0 0 0 255 10) ;  0000FF toma 0 0 255
                   (pixrgb-d 1 1 255 255 255 1) ;FFFFFF toma 255 255 255
  ))

(define img3 (image  2
                   (pixrgb-d 0 0 255 0 0 10) ;  FF0000 toma 255 0 0         ;#f
                   (pixrgb-d 0 1 0 255 0 20) ;  00FF00 toma 0 255 0
                   (pixrgb-d 1 0 0 0 255 10) ;  0000FF toma 0 0 255
                   (pixrgb-d 1 1 255 255 255 1) ;FFFFFF toma 255 255 255
  ))

(define img4 (image 2 2
                   (pixrgb-d 0 0 255 0 0 10) ;  FF0000 toma 255 0 0           ;#t
                   (pixrgb-d 0 1 0 255 0 20) ;  00FF00 toma 0 255 0
                   (pixrgb-d 1 0 0 0 255 10) ;  0000FF toma 0 0 255
                   ;(pixrgb-d 1 1 255 255 255 1) ;FFFFFF toma 255 255 255
  ))

;(image 5 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)
 ))

;(entregar_coordenadas (coordenadas 2 2 0 0 null) (acceder img1 2) (contar (acceder img1 2)) 0 0 null)

;(acceder img1 2)




;'('(1 2 3 4) '(1 2 3 '(6 7 8) 5 6 7 8 9))


;(append (acceder '(((1 2 3 4) (1 2 3 '(6 7 8) 5 6 7 8 9))) 0))





; Dominio: 
; Recorrido: 
; Descripcion:
; Tipo de recursion: No se utiliza recursion


; Dominio: 
; Recorrido: 
; Descripcion:
; Tipo de recursion: 


;-----------------------------------OTRAS FUNCIONES-------------------------------------------------------------------


;(cons 7(cons (acceder(acceder img1 2)0) null))


;Esta función es utilizada para hacer uso del TDA en el archivo main.rkt
(provide (all-defined-out))