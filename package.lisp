;;;; package.lisp — package definition for jpeg-sixel

(defpackage :jpeg-sixel
  (:use :cl)
  (:documentation
   "Convert JPEG images to sixel escape sequences for terminal display.
    Built on cl-jpeg. Provides quantization, Floyd-Steinberg dithering,
    downscale-to-fit, and a terminal cell-size probe for column-accurate sizing.")
  (:export
   ;; core
   #:jpeg->sixel
   #:write-jpeg-sixel
   ;; terminal probe
   #:query-cell-size
   #:columns-for-width
   #:sixel-supported-p))
