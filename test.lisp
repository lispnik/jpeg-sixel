;;;; test.lisp — tests for jpeg-sixel
;;;;
;;;; Two layers:
;;;;   * unit tests on internals (median-cut invariants, emit-sixel exact bytes,
;;;;     emit-run RLE, fit-dimensions) — these pin behavior and guard refactors
;;;;     such as the median-cut rewrite.
;;;;   * end-to-end tests that encode a fixture JPEG (built in-process via
;;;;     cl-jpeg, so no external files) and check the sixel envelope, geometry,
;;;;     palette, band structure, and the downscale/dither options.
;;;;
;;;; Note: cl-jpeg's encoder rejects 1-component images, so the grayscale
;;;; (ncomp=1) decode branch of jpeg->sixel is not exercised here.

(defpackage :jpeg-sixel-test
  (:use :cl)
  (:export #:run-tests))
(in-package :jpeg-sixel-test)

(defvar *failures* 0)
(defvar *checks* 0)

(defmacro check (form &optional (msg nil))
  `(progn
     (incf *checks*)
     (handler-case
         (if ,form
             (format t "  ok   ~a~%" (or ,msg ',form))
             (progn (incf *failures*)
                    (format t "  FAIL ~a~%" (or ,msg ',form))))
       (error (e)
         (incf *failures*)
         (format t "  FAIL ~a  [signalled ~a]~%" (or ,msg ',form) e)))))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun make-test-jpeg (path &key (h 64) (w 96))
  "Encode a small BGR gradient JPEG via cl-jpeg so tests need no fixtures."
  (let* ((ncomp 3)
         (img (make-array (* h w ncomp) :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (dotimes (x w)
        (let ((o (* (+ (* y w) x) ncomp)))
          ;; cl-jpeg wants BGR
          (setf (aref img (+ o 0)) (mod (* x 2) 256)   ; B
                (aref img (+ o 1)) (mod (* y 3) 256)   ; G
                (aref img (+ o 2)) (mod (+ x y) 256)))))  ; R
    (jpeg:encode-image path img ncomp h w)
    (values w h)))

(defun count-char (ch s)
  (count ch s))

(defun raster-dims (sixel)
  "Pull (values w h) from the sixel raster-attribute header (\"1;1;W;H), or NIL."
  (let ((qpos (position #\" sixel)))
    (when qpos
      (let ((nums '()) (start (1+ qpos)) (n 0) (len (length sixel)))
        (loop while (and (< start len) (< n 4)) do
          (multiple-value-bind (val end)
              (parse-integer sixel :start start :junk-allowed t)
            (unless val (return))
            (push val nums) (incf n)
            (setf start (if (and (< end len) (char= (char sixel end) #\;))
                            (1+ end) end))
            (when (and (< start len) (not (digit-char-p (char sixel start))))
              (unless (char= (char sixel (1- start)) #\;) (return)))))
        (let ((r (nreverse nums)))
          (when (>= (length r) 4)
            (values (third r) (fourth r))))))))

(defun count-color-regs (sixel)
  "Number of palette register definitions, matched as #<digits>;2; anchored on
   the leading '#'. (A bare ';2;' search over-counts: a channel value of 2
   yields a spurious ';2;'. Color-selects '#n' are followed by sixel data, not
   ';2;', so anchoring on '#...;2;' counts only real register defs.)"
  (let ((n 0) (i 0) (len (length sixel)))
    (loop for h = (position #\# sixel :start i) while h do
      (multiple-value-bind (num e) (parse-integer sixel :start (1+ h) :junk-allowed t)
        (when (and num e (<= (+ e 3) len)
                   (string= ";2;" (subseq sixel e (+ e 3))))
          (incf n))
        (setf i (1+ h))))
    n))

(defun cluster-image (clusters side)
  "Build (values r g b npix) for a SIDE x SIDE image whose columns are split
   evenly among CLUSTERS (a list of (r g b)). Pixels in a cluster are that exact
   color, so a K-color quantization should recover all K colors."
  (let* ((k (length clusters))
         (w (* side k)) (h side) (npix (* w h))
         (r (make-array npix :element-type '(unsigned-byte 8)))
         (g (make-array npix :element-type '(unsigned-byte 8)))
         (b (make-array npix :element-type '(unsigned-byte 8)))
         (vec (coerce clusters 'vector)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((i (+ (* y w) x)))
          (destructuring-bind (rr gg bb) (aref vec (floor x side))
            (setf (aref r i) rr (aref g i) gg (aref b i) bb)))))
    (values r g b npix)))

;;; ---------------------------------------------------------------------------
;;; Unit tests: internals
;;; ---------------------------------------------------------------------------

(defun test-emit-run ()
  (format t "~&emit-run (RLE):~%")
  (flet ((run (ch count)
           (with-output-to-string (s) (jpeg-sixel::emit-run s ch count))))
    (check (string= "" (run #\@ 0)) "count 0 emits nothing")
    (check (string= "@" (run #\@ 1)) "count 1 literal")
    (check (string= "@@@" (run #\@ 3)) "count 3 literal (no RLE)")
    (check (string= "!4@" (run #\@ 4)) "count 4 uses RLE")
    (check (string= "!100A" (run #\A 100)) "large run uses RLE")))

(defun test-emit-sixel-exact ()
  "Pin the exact bytes of emit-sixel on a tiny hand-checkable input."
  (format t "~&emit-sixel (exact bytes):~%")
  (let* ((palette (vector '(0 0 0) '(255 255 255)))
         (index (make-array 2 :element-type '(unsigned-byte 8)
                              :initial-contents '(0 1)))
         (got (jpeg-sixel::emit-sixel palette index 2 1))
         (esc (code-char 27))
         (want (concatenate 'string
                            (string esc) "Pq"
                            "\"1;1;2;1"
                            "#0;2;0;0;0" "#1;2;100;100;100"
                            "#0@?" "$" "#1?@" "-"
                            (string esc) "\\")))
    (check (string= got want)
           (format nil "2x1 black/white emit-sixel matches exact expected"))
    (unless (string= got want)
      (format t "    want: ~s~%    got:  ~s~%" want got))))

(defun test-fit-dimensions ()
  (format t "~&fit-dimensions:~%")
  (flet ((fit (sw sh mw mh) (multiple-value-list
                             (jpeg-sixel::fit-dimensions sw sh mw mh))))
    (check (equal '(100 50) (fit 100 50 nil nil)) "no bounds: unchanged")
    (check (equal '(100 50) (fit 100 50 200 200)) "bounds larger: never upscales")
    (check (equal '(50 25) (fit 100 50 50 nil)) "width bound halves, keeps aspect")
    (check (equal '(50 25) (fit 100 50 nil 25)) "height bound halves, keeps aspect")
    (check (destructuring-bind (w h) (fit 100 50 40 40)
             (and (<= w 40) (<= h 40))) "both bounds respected")
    (check (destructuring-bind (w h) (fit 3 3 1 1)
             (and (>= w 1) (>= h 1))) "never collapses below 1px")))

(defun test-median-cut-invariants ()
  "The core correctness guard for the quantizer: palette bound, indices in
   range and complete, and faithful reconstruction of well-separated clusters."
  (format t "~&median-cut invariants:~%")
  ;; 1. cluster recovery + reconstruction fidelity
  (let ((clusters '((240 20 20) (20 240 20) (20 20 240) (200 200 200))))
    (multiple-value-bind (r g b npix) (cluster-image clusters 6)
      (multiple-value-bind (palette index) (jpeg-sixel::median-cut r g b npix 4)
        (check (<= (length palette) 4) "palette respects ncolors bound")
        (check (= (length index) npix) "index length = pixel count")
        (check (every (lambda (s) (< s (length palette))) index)
               "every index in palette range")
        ;; every pixel reconstructs near its true color (catches dropped/
        ;; misassigned pixels in the partition)
        (let ((max-err 0))
          (dotimes (i npix)
            (destructuring-bind (pr pg pb) (aref palette (aref index i))
              (let ((e (max (abs (- pr (aref r i)))
                            (abs (- pg (aref g i)))
                            (abs (- pb (aref b i))))))
                (when (> e max-err) (setf max-err e)))))
          (check (< max-err 30)
                 (format nil "all pixels reconstruct within tolerance (max err ~d)"
                         max-err))
          ;; each source cluster color has a near palette entry
          (check (every (lambda (c)
                          (some (lambda (p)
                                  (destructuring-bind (pr pg pb) p
                                    (destructuring-bind (cr cg cb) c
                                      (< (+ (abs (- pr cr)) (abs (- pg cg))
                                            (abs (- pb cb)))
                                         30))))
                                (coerce palette 'list)))
                        clusters)
                 "each cluster color present in palette")))))
  ;; 2. degenerate: fewer distinct colors than ncolors still bounded & valid
  (multiple-value-bind (r g b npix) (cluster-image '((10 10 10) (250 250 250)) 4)
    (multiple-value-bind (palette index) (jpeg-sixel::median-cut r g b npix 16)
      (check (<= (length palette) 16) "over-provisioned ncolors stays bounded")
      (check (every (lambda (s) (< s (length palette))) index)
             "indices valid with duplicate-color boxes")))
  ;; 3. single pixel
  (multiple-value-bind (r g b npix) (cluster-image '((123 45 67)) 1)
    (multiple-value-bind (palette index) (jpeg-sixel::median-cut r g b npix 8)
      (check (and (>= (length palette) 1) (= (length index) 1))
             "single-pixel image quantizes")
      (check (equal '(123 45 67) (aref palette (aref index 0)))
             "single pixel color preserved exactly")))
  ;; 4. determinism
  (multiple-value-bind (r g b npix) (cluster-image '((1 2 3) (4 5 6) (7 8 9)) 5)
    (multiple-value-bind (p1 i1) (jpeg-sixel::median-cut r g b npix 8)
      (multiple-value-bind (p2 i2) (jpeg-sixel::median-cut r g b npix 8)
        (check (and (equalp p1 p2) (equalp i1 i2))
               "median-cut is deterministic")))))

;;; ---------------------------------------------------------------------------
;;; End-to-end tests
;;; ---------------------------------------------------------------------------

(defun test-end-to-end (tmp w h)
  (format t "~&Encoding (~dx~d fixture):~%" w h)
  ;; envelope
  (let ((s (jpeg-sixel:jpeg->sixel tmp :max-colors 64 :dither nil)))
    (check (and (char= (char s 0) #\Escape) (char= (char s 1) #\P))
           "starts with DCS (ESC P)")
    (check (and (char= (char s (- (length s) 2)) #\Escape)
                (char= (char s (1- (length s))) #\\))
           "ends with ST (ESC backslash)")
    (multiple-value-bind (rw rh) (raster-dims s)
      (check (and (eql rw w) (eql rh h)) "raster dims match source"))
    ;; band structure: one '-' terminator per 6-row band
    (check (= (count-char #\- s) (ceiling h 6))
           (format nil "band count = ceil(h/6) = ~d" (ceiling h 6)))
    ;; palette registers bounded by max-colors
    (check (<= (count-color-regs s) 64) "palette regs <= max-colors")
    (check (>= (count-color-regs s) 1) "at least one palette reg"))
  ;; max-colors clamp to 256
  (let ((s (jpeg-sixel:jpeg->sixel tmp :max-colors 1000 :dither nil)))
    (check (<= (count-color-regs s) 256) "max-colors clamped to 256"))
  ;; dithered path well-formed
  (let ((s (jpeg-sixel:jpeg->sixel tmp :max-colors 16 :dither t)))
    (check (and (char= (char s 0) #\Escape)
                (char= (char s (1- (length s))) #\\))
           "dithered output well-formed")
    (check (= (count-char #\- s) (ceiling h 6)) "dithered band count correct"))
  ;; downscale by width, aspect preserved
  (let ((s (jpeg-sixel:jpeg->sixel tmp :max-width 48 :dither nil)))
    (multiple-value-bind (rw rh) (raster-dims s)
      (check (and rw (<= rw 48) (< rw w)) "max-width downscales")
      (check (and rh (= rh (floor (* h rw) w))) "downscale preserves aspect")))
  ;; downscale by height
  (let ((s (jpeg-sixel:jpeg->sixel tmp :max-height 24 :dither nil)))
    (multiple-value-bind (rw rh) (raster-dims s)
      (declare (ignore rw))
      (check (and rh (<= rh 24) (< rh h)) "max-height downscales")))
  ;; :cols convenience sizing (no tty -> fallback 10px cell)
  (let ((s (jpeg-sixel:jpeg->sixel tmp :cols 4 :dither nil)))
    (multiple-value-bind (rw rh) (raster-dims s)
      (declare (ignore rh))
      (check (and rw (<= rw 40)) ":cols bounds width to cols*cell")))
  ;; write-jpeg-sixel matches jpeg->sixel
  (let ((direct (jpeg-sixel:jpeg->sixel tmp :max-colors 32 :dither nil))
        (viastream (with-output-to-string (o)
                     (jpeg-sixel:write-jpeg-sixel tmp o :max-colors 32 :dither nil))))
    (check (string= direct viastream) "write-jpeg-sixel matches jpeg->sixel")))

(defun test-probe ()
  (format t "~&Terminal probe (no tty / batch):~%")
  (check (equal '(nil nil)
                (multiple-value-list (jpeg-sixel:query-cell-size :timeout-decisec 2)))
         "query-cell-size returns NIL NIL with no tty")
  (check (= 120 (jpeg-sixel:columns-for-width 1200 :default-cell-w 10))
         "columns-for-width fallback")
  (check (eq :unknown (jpeg-sixel:sixel-supported-p :timeout-decisec 2))
         "sixel-supported-p returns :unknown with no tty")
  (check (member 4 (jpeg-sixel::%parse-da-features
                    (format nil "~c[?62;4;6;22c" #\Escape)))
         "DA parser finds sixel code 4")
  (check (not (member 4 (jpeg-sixel::%parse-da-features
                         (format nil "~c[?62;22c" #\Escape))))
         "DA parser rejects when 4 absent")
  (check (null (jpeg-sixel::%parse-da-features "garbage"))
         "DA parser rejects malformed reply"))

;;; ---------------------------------------------------------------------------

(defun run-tests ()
  (setf *failures* 0 *checks* 0)
  (test-emit-run)
  (test-emit-sixel-exact)
  (test-fit-dimensions)
  (test-median-cut-invariants)
  (test-probe)
  (let ((tmp (merge-pathnames "jpeg-sixel-test.jpg" (uiop:temporary-directory))))
    (unwind-protect
         (multiple-value-bind (w h) (make-test-jpeg tmp)
           (test-end-to-end tmp w h))
      (ignore-errors (delete-file tmp))))
  (format t "~&~%~d checks, ~[all passed.~:;~:*~d failure(s).~]~%"
          *checks* *failures*)
  (when (> *failures* 0)
    (error "jpeg-sixel tests failed: ~d/~d" *failures* *checks*))
  t)
