#lang racket
; Se requiere el uso del TDA image, TDA bitmap, TDA hexmap y TDA pixmap
(require "otras_funciones.rkt")
(require "bitmap.rkt")
(require "hexmap.rkt")
(require "pixmap.rkt")

;-----------------------------------TDA Image------------------------------------------------------------------------


;-----------------------------------REPRESENTACION--------------------------------------------------------------------

; Este TDA representa imagenes compuestas por bitmaps, hexmaps o pixmaps, además, representa la operaciones que se pueden
; hacer en cada foto mediante el uso y la modificación de pixeles. Una imagen está compuesta por un ancho, un alto y los pixeles, en
;donde cada pixel (lista) queda almacenado dentro de una lista.

;Ej: pixel = (pixbit-d 0 0 1 10) -> (0 0 1 10)

;Ej: imagen = lista de pixeles = (pixbit-d 0 0 0 10)(pixbit-d 0 1 1 20)(pixbit-d 1 0 1 10)(pixbit-d 1 1 0 255) -> '((0 0 0 10) (0 1 1 20) (1 0 1 10) (1 1 0 255))

;-----------------------------------CONSTRUCTOR---------------------------------------------------------------------
; Dominio: Ancho (int) X alto (int) X [pixbit-d* | pixrgb-d* | pixhex]
; Recorrido: Image (list)
; Descripcion: Crea una imagen en base a pixeles 
; Tipo de recursion: No utiliza recursión
(define (image ancho alto . pixeles)
  (list ancho alto pixeles))


;Función de pertenencia
;----------------------------------------------------Compressed?----------------------------------------------------
; Dominio: image
; Recorrido: boolean
; Descripcion: Comprueba que la cantidad de pixeles no haya sido comprimida. Retorna #f si no está comprimida, retorna #t si esta comprimida y da aviso en caso de haber ingresado mas pixeles de los que debería
; Tipo de recursion: acceder utiliza recursión de cola
(define (compressed? lista)
  (cond
    [(< (tamanio lista) (contar (acceder lista 2))) print "Hay mas pixeles que el tamanio declarado"]
    [(= (tamanio lista) (contar (acceder lista 2))) #f];no comprimida
    [(> (- (tamanio lista) 1) (contar (acceder lista 2))) print "No se puede volver a comprimir"]
    [(= (- (tamanio lista) 1) (contar (acceder lista 2))) #t]));comprimida


;-------------------------------------------------------------------------------------------------------------------




;----------------------------------------------------------------MODIFICADORES-----------------------------------------------------------


;-------------------------------------------------------flipH-------------------------------------------------------
; Dominio: ancho (int) X alto (int) X cont1 (int) X cont1 (int) X lista (list)
; Recorrido: list
; Descripcion: Se generan todas las coordenadas posibles dependiendo del ancho y del alto
; Tipo de recursion: Recursión de cola
(define (coordenadas ancho alto cont1 cont2 lista)
  (cond
    [(= alto cont1) (reverse lista)]
    [(= ancho cont2) (coordenadas ancho alto (+ 1 cont1) 0 lista)]
    [else (coordenadas ancho alto cont1 (+ 1 cont2) (cons cont2 (cons cont1 lista)))]))


; Dominio: lista (list) X contador (int)
; Recorrido: list
; Descripcion: LLama a la función eliminar para quitar los dos primero elementos de una lista, los cuales corresponden a coordenadas
; Tipo de recursion: Recursión de cola
(define (eliminar_coordenadas lista contador)
  (cond
    [(< contador 1) (eliminar_coordenadas (eliminar lista (acceder lista 0)) (+ contador 1))]
    [else lista]))


; Dominio: lista_coordenadas (list) X pixel (list) X contador (int) X posición (int)
; Recorrido: list
; Descripcion: Cambia las coordenadas de un pixel
; Tipo de recursion: Recursión de cola
(define (eliminar_e_insertar lista_coordenadas pixel contador posicion)
  (cond
    [(< contador 2) (eliminar_e_insertar lista_coordenadas (eliminar_coordenadas pixel 0) (+ contador 1) posicion)]
    [else (cons (acceder lista_coordenadas posicion)(cons (acceder lista_coordenadas (+ posicion 1)) pixel))]))


; Dominio: lista_coordenadas (list) X lista_pixeles (list) X cantidad (int) X contador (int) X contador2 (int) nueva_lista (list)
; Recorrido: list
; Descripcion: Llama a la función para eliminar las coordenadas anteriores y agregar las nuevas
; Tipo de recursion: Recursión de cola
(define (entregar_coordenadas lista_coordenadas lista_pixeles cantidad contador contador2 nueva_lista)
  (cond
    [(and (not (= contador cantidad)) (<= contador2 (+ cantidad cantidad))) (entregar_coordenadas lista_coordenadas lista_pixeles cantidad (+ contador 1) (+ contador2 2) (cons (eliminar_e_insertar lista_coordenadas (acceder lista_pixeles contador) 0 contador2) nueva_lista))]
    [else (reverse nueva_lista)]))


; Dominio: lista (list) X contador (int) X salida (list)
; Recorrido: list
; Descripcion: invierte un tramo de la lista y lo agrega al final
; Tipo de recursion: Recursión natural
(define (invertir lista contador salida)
  (cond
    [(= contador salida) lista]
    [else (append (invertir (cdr lista) (- contador 1) salida) (list (car lista)) )]))


; Dominio: lista (list) X cantidad (int) X contador (int) X ancho (int)
; Recorrido: list
; Descripcion: LLama a la funcion invertir listas 
; Tipo de recursion: Recursión de cola
(define (llamar_inversion lista cantidad contador ancho)
 (cond
   [(= contador 0) lista]
   [else (llamar_inversion (invertir lista cantidad (- cantidad ancho)) (- cantidad ancho) (- contador ancho) ancho)]))


; Dominio: lista (list)
; Recorrido: imagen
; Descripcion: Invierte una imagen horizontalmente
; Tipo de recursion: entregar_coordenadas utiliza recursión de cola
(define (flipH lista)
  (cond
    [(not (null? lista)) (entregar_coordenadas (coordenadas (acceder lista 0) (acceder lista 1) 0 0 null) (llamar_inversion (acceder lista 2) (contar (acceder lista 2)) (tamanio lista) (acceder lista 0)) (contar (acceder lista 2)) 0 0 null)]))

;(flipH (image 3 3 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5) (pixbit-d 6 6 6 6) (pixbit-d 7 7 7 7) (pixbit-d 8 8 8 8)))
;1 0 3 2

;(flipH (image 2 3 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5)))

;-------------------------------------------------------------------------------------------------------------------


;-------------------------------------------------------flipV-------------------------------------------------------

; Dominio: lista (list) X ancho (int) X contador (int) X lista_aux (list) X lista_aux2 (list)
; Recorrido: list
; Descripcion: Forma sub-listas del tamaño que el ancho de la imagen
; Tipo de recursion: Recursión de cola
(define (sub-listas lista ancho contador lista_aux lista_aux2)
  (cond
    [(null? lista) (reverse(cons (reverse lista_aux) lista_aux2))]
    [(= ancho contador) (sub-listas (cdr lista) ancho 1 (cons (car lista) null)(cons (reverse lista_aux) lista_aux2))]
    [else (sub-listas (cdr lista) ancho (+ contador 1) (cons (car lista) lista_aux) lista_aux2)]))

  
; Dominio: lista (list)
; Recorrido: imagen
; Descripcion: Invierte una imagen verticalmente
; Tipo de recursion: llamar inversión utiliza recursión de cola
(define (flipV lista)
  (cond                       
    [(not (null? lista)) (reverse(sub-listas (acceder lista 2) (acceder lista 0) 0 null null))]))

;(flipV (image 3 2 (pixbit-d 0 0 0 0) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5)))
;3 4 5 0 1 2
;-------------------------------------------------------------------------------------------------------------------



;--------------------------------------------------------crop-------------------------------------------------------
; Dominio: lista (list) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: Boolean
; Descripcion: Verifica si un pixel esta dentro del cuadrante dependiendo de las coordenadas x1, y1, x2 e y2 en cada pixel 
; Tipo de recursion: No se utiliza recursión
(define (esta? lista x1 y1 x2 y2)
  (cond
    [(and (and (>= (acceder (acceder lista 0)0) x1) (<= (acceder (acceder lista 0)0) x2)) (and (>= (acceder (acceder lista 0)1) y1) (<= (acceder (acceder lista 0)1) y2))) 1]
    [else 0]))


; Dominio: lista (list) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: List
; Descripcion: Enlista las coordenadas verificadas
; Tipo de recursion: Recursión de cola
(define (verificar_coordenadas lista x1 y1 x2 y2 lista_auxiliar)
  (cond
    [(null? lista) (reverse lista_auxiliar)]
    [(= 1 (esta? lista x1 y1 x2 y2)) (verificar_coordenadas (cdr lista) x1 y1 x2 y2 (cons (car lista) lista_auxiliar))]
    [else (verificar_coordenadas lista x1 y1 x2 y2 lista_auxiliar)]))


; Dominio: lista (list) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: imagen
; Descripcion: Llama a la función para verificar coordenadas para retornar una imagen recortada
; Tipo de recursion: Verificar_coordenadas utiliza recursión de cola
(define (crop lista x1 y1 x2 y2)
  (cond
    [(not (and (< (- x2 x1) 3) (< (- y2 y1) 3))) (verificar_coordenadas (acceder lista 2) x1 y1 x2 y2 null)]
    [else print "Debe seleccionar un area mas grande"]))

;-------------------------------------------------------------------------------------------------------------------


;---------------------------------------------------imgRGB->imgHex--------------------------------------------------
; Dominio: color (int) X lista (list) X valor1 (int) X valor2 (int) X contador (int)
; Recorrido: string
; Descripcion: Transforma un número RGB a Hexadecimal
; Tipo de recursion: Recursión de cola
(define (color->hex color lista_hexadecimal valor1 valor2 contador)
  (cond
    [(< contador 1) (color->hex color lista_hexadecimal (modulo color 16) (modulo (quotient color 16) 16) (+ contador 1))]
    [else (string-append (acceder lista_hexadecimal valor2) (acceder lista_hexadecimal valor1))]))


; Dominio: lista (list) X contador (int) X profundidad (int) X r (int) X g (int) X b (int)
; Recorrido: list
; Descripcion: Cambia el formato de un pixel. Transforma de pixmap a hexmap
; Tipo de recursion: Recursión natural
(define (cambiar_formato lista contador profundidad r g b)
  (cond
    [(< contador 4) (cambiar_formato (eliminar lista (acceder lista 2)) (+ contador 1) profundidad r g b)]
    [else (cons (car lista)(cons (cadr lista)(cons (string-append(color->hex r Hexadecimal 0 0 0) (color->hex g Hexadecimal 0 0 0) (color->hex b Hexadecimal 0 0 0)) (cons profundidad null))))]))


; Dominio: lista (list) X lista (list)
; Recorrido: List
; Descripcion: Entrega pixel por pixel a cambiar_formato para que los modifique
; Tipo de recursion: Recursión de cola
(define (lista_a_Hexadecimal lista lista_auxiliar)
  (cond
    [(not(null? lista)) (lista_a_Hexadecimal (cdr lista) (cons (cambiar_formato (car lista) 0 (acceder (car lista) 5) (acceder (car lista) 2) (acceder (car lista) 3) (acceder (car lista) 4)) lista_auxiliar))]
    [else (reverse lista_auxiliar)]))



; Dominio: image
; Recorrido: image
; Descripcion: Realiza el llamado a la funcion que transforma los rgb en hex
; Tipo de recursion: entregar_coordenas utiliza recursión de cola
(define (imgRGB->imgHex lista)
  (cond
    [(pixmap? lista) (entregar_coordenadas (coordenadas (acceder lista 0) (acceder lista 1) 0 0 null) (lista_a_Hexadecimal (acceder lista 2) null) (contar (acceder lista 2)) 0 0 null)]
    [else print "La imagen ingresada no es del tipo pixmap"]))
;--------------------------------------------------------------------------------------------------------------------



;------------------------------------------------------Rotate90-----------------------------------------------------

; Dominio: lista (list) X cantidad (int) X contador (int) X lista (list) 
; Recorrido: lista (list)
; Descripcion: Enlista los elementos en sub-lista del tamaño contrario al que se había generado previamente
; Tipo de recursion: Recursión de cola
(define (enlistar_car lista cantidad contador lista_aux)
  (cond
    [(null? lista) (reverse lista_aux)]
    [(not(= cantidad contador)) (enlistar_car (cdr lista) cantidad (+ contador 1) (cons (acceder(acceder lista 0)0) lista_aux))]))


; Dominio: lista (list) X cantidad (int) X contador (int) X lista (list)
; Recorrido: 
; Descripcion: Enlista los elementos que aun no han sido manipulados con enlistar_car
; Tipo de recursion: Recursión de cola
(define (enlistar_cdr lista cantidad contador lista_aux)
  (cond
    [(null? lista) (reverse lista_aux)]
    [(not(= cantidad contador)) (enlistar_cdr (cdr lista) cantidad (+ contador 1) (cons (cdr (acceder lista 0)) lista_aux))]))



; Dominio: lista (list) X contador (int) X cantidad (int) X lista (list)
; Recorrido: lista (list)
; Descripcion: Invierte las filas de una matriz, pero cuando ya está formada la lista invierte algo así ((1 2 3 4)(5 6 7 8))-> ((4 3 2 1)(8 7 6 5))
; Tipo de recursion: Recursión de cola
(define (invertir_filas lista contador cantidad lista_aux)
  (cond
    [(< contador cantidad) (invertir_filas lista (+ contador 1) cantidad (cons (reverse(acceder lista contador)) lista_aux))]
    [else (reverse lista_aux)]))


; Dominio: lista (list)
; Recorrido: list (list)
; Descripcion: LLama a la función para realizar las rotaciones
; Tipo de recursion: enlistar_car utiliza recursión de cola
(define (llamar_rotacion lista)
  (cond
    [(null? (car lista)) '()]
    [else (cons (enlistar_car lista (contar lista) 0 null) (llamar_rotacion (enlistar_cdr lista (contar lista) 0 null)))]))


; Dominio: lista (list)
; Recorrido: imagen
; Descripcion: Llama la funciones para invertir la imagen 90 grados
; Tipo de recursion: invertir_filas utiliza recursion de cola
(define (rotate90 lista)
  (cond
    [(not (null? lista)) (invertir_filas (llamar_rotacion (sub-listas (acceder lista 2) (acceder lista 0) 0 null null)) 0 (contar (llamar_rotacion (sub-listas (acceder lista 2) (acceder lista 0) 0 null null))) null)]))
;--------------------------------------------------------------------------------------------------------------------

;Funcion de pertenecia
;------------------------------------------------------Histogram-----------------------------------------------------
; Dominio: lista (list) X ceros (int) X unos (int) X lista (list)
; Recorrido: lista (list)
; Descripcion: Retorna una lista con sub-listas. Las sub-listas sigue la estructura ((0 cantidad_de_pixeles_0)(1 cantidad_de_pixeles_1))
; Tipo de recursion: Recursión de cola
(define (contar_bit lista ceros unos lista_aux)
  (cond
    [(null? lista) (cons (list 0 ceros)(cons (list 1 unos) null))]
    [(= 0 (acceder (acceder lista 0) 2)) (contar_bit (cdr lista) (+ ceros 1) unos lista_aux)]
    [(= 1 (acceder (acceder lista 0) 2)) (contar_bit (cdr lista) ceros (+ unos 1) lista_aux)]))

; Dominio: image
; Recorrido: lista (list)
; Descripcion: Realiza un histograma de los bits
; Tipo de recursion: contar_bit utiliza recursión de cola
(define (Histogram lista)
  (cond
    [(bitmap? lista) (contar_bit (acceder lista 2) 0 0 null)]))
;--------------------------------------------------------------------------------------------------------------------


;----------------------------------------------------------------------------OTRAS FUNCIONES------------------------------------------------------------------

;Esta función es utilizada para hacer uso del TDA en el archivo main.rkt
(provide (all-defined-out))