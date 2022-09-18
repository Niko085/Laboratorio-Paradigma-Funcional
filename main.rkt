#lang racket
; Se requiere el uso del TDA image
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
(hexmap? (image 5 3 (pixhex-d 0 255 "nico" 0) (pixhex-d 255 56 "ugu" 1)))
(hexmap? img1)