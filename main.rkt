#lang racket
; Se requiere el uso del TDA image
(require "TDAImage.rkt")
(require "bitmap.rkt")
(require "hexmap.rkt")
(require "pixmap.rkt")
(bitmap? (image 5 3 (pixbit-d 0 0 1 0) (pixbit-d 1 6 0 1)))