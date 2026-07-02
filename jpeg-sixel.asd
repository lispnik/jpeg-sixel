;;;; jpeg-sixel.asd

(asdf:defsystem :jpeg-sixel
  :name "jpeg-sixel"
  :version "0.1.0"
  :license "MIT"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :description "Convert JPEG images to sixel escape sequences for terminal display."
  :long-description
  "A small, dependency-light JPEG-to-sixel encoder built on cl-jpeg. Handles
   median-cut color quantization, Floyd-Steinberg dithering with a cached
   nearest-color lookup, box-average downscale-to-fit, and a best-effort
   terminal cell-size probe (XTerm 14t/18t) so images can be sized to a target
   number of terminal columns."
  :depends-on (:cl-jpeg)
  :serial t
  :components ((:file "package")
               (:file "jpeg-sixel")
               (:file "terminal-probe"))
  :in-order-to ((asdf:test-op (asdf:test-op :jpeg-sixel/test))))

(asdf:defsystem :jpeg-sixel/test
  :description "Smoke tests for jpeg-sixel."
  :depends-on (:jpeg-sixel)
  :serial t
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :jpeg-sixel-test :run-tests)))
