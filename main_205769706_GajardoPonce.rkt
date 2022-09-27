#lang racket
; Se requiere el uso del TDA image, TDA bitmap, TDA hexmap y TDA pixmap
(require "otras_funciones.rkt")
(require "TDAImage.rkt")
(require "bitmap.rkt")
(require "hexmap.rkt")
(require "pixmap.rkt")

;---------------------Pruebas-----------------------
print "bitmap?"
(bitmap? (image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1)))
(bitmap? img1)

print "pixmap?"
(pixmap? (image 5 3 (pixrgb-d 0 255 255 0 0 0) (pixrgb-d 255 56 255 1 9 8)))
(pixmap? img1)

print "hexmap?"
(hexmap? (image 5 3 (pixhex-d 0 255 "FFFF" 0) (pixhex-d 255 56 "FFFF" 1)))
(hexmap? img1)

print"compressed?"
(compressed? img1)
(compressed? (image 2 3 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))
(compressed? (image 1 2 (pixrgb-d 0 0 255 255 255 0) (pixrgb-d 9 8 255 56 255 1)))
(compressed? img4)

print"flipH"
(flipH img2)
(flipH (image 2 3 (pixbit-d 0 0 0 2) (pixbit-d 0 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5)))
(flipH (image 3 3 (pixbit-d 0 0 0 2) (pixbit-d 1 1 1 1 ) (pixbit-d 2 2 2 2) (pixbit-d 3 3 3 3) (pixbit-d 4 4 4 4) (pixbit-d 5 5 5 5) (pixbit-d 6 6 6 6) (pixbit-d 7 7 7 7) (pixbit-d 8 8 8 8)))

print"flipV"
(flipV img2)

print"crop"
(crop img2 0 0 3 3)

print"imgRGB->imgHex"
(imgRGB->imgHex img1)

print"Histogram"
(Histogram img2)


(define img10 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255))
 )

(crop img2 0 0 3 3)
(rotate90 img2)



;(coordenadas 4 4 0 0 null)
;(crop img2 0 0 3 3)