;;;; bench.lisp — performance benchmark for jpeg-sixel.
;;;;
;;;; Times each pipeline stage on a deterministic synthetic image so runs are
;;;; comparable before/after optimization work. Load via the jpeg-sixel/bench
;;;; system and call (jpeg-sixel-bench:run-bench).

(defpackage :jpeg-sixel-bench
  (:use :cl)
  (:export #:run-bench))
(in-package :jpeg-sixel-bench)

(defun ms (internal-ticks)
  "Convert internal-run-time ticks to milliseconds (single-float)."
  (/ (float internal-ticks 1.0)
     (/ internal-time-units-per-second 1000.0)))

(defmacro bench ((label reps) &body body)
  "Run BODY REPS times, report best and mean wall-ms. Returns last value."
  (let ((r (gensym)) (best (gensym)) (total (gensym))
        (t0 (gensym)) (dt (gensym)) (val (gensym)) (i (gensym)))
    `(let ((,best most-positive-fixnum) (,total 0) (,val nil))
       (dotimes (,i ,reps)
         (let ((,t0 (get-internal-run-time)))
           (setf ,val (progn ,@body))
           (let ((,dt (- (get-internal-run-time) ,t0)))
             (when (< ,dt ,best) (setf ,best ,dt))
             (incf ,total ,dt))))
       (format t "  ~28a  best ~8,2f ms   mean ~8,2f ms~%"
               ,label (ms ,best) (ms (/ ,total ,reps)))
       ,val)))

(declaim (inline clamp8))
(defun clamp8 (x) (max 0 (min 255 x)))

(defun gen-image (w h)
  "Deterministic pseudo-photographic image: smooth RGB gradients plus a hashed
   per-pixel jitter so there are thousands of distinct colors (real work for
   median-cut). Returns (values r g b) as (unsigned-byte 8) arrays of length W*H."
  (declare (type fixnum w h))
  (let* ((n (* w h))
         (r (make-array n :element-type '(unsigned-byte 8)))
         (g (make-array n :element-type '(unsigned-byte 8)))
         (b (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((i (+ (* y w) x))
               (br (floor (* 255 x) w))
               (bg (floor (* 255 y) h))
               (bb (floor (* 255 (+ x y)) (+ w h)))
               (hash (logand (+ (* x 1103515245) (* y 12345)
                                (* x y 2654435761) 987654321)
                             #xffffff))
               (j1 (- (logand hash 31) 16))
               (j2 (- (logand (ash hash -5) 31) 16))
               (j3 (- (logand (ash hash -10) 31) 16)))
          (setf (aref r i) (clamp8 (+ br j1))
                (aref g i) (clamp8 (+ bg j2))
                (aref b i) (clamp8 (+ bb j3))))))
    (values r g b)))

(defun write-bench-jpeg (path w h)
  "Encode the synthetic image to a baseline JPEG at PATH (BGR interleave)."
  (multiple-value-bind (r g b) (gen-image w h)
    (let ((img (make-array (* w h 3) :element-type '(unsigned-byte 8))))
      (dotimes (i (* w h))
        (let ((o (* i 3)))
          (setf (aref img (+ o 0)) (aref b i)
                (aref img (+ o 1)) (aref g i)
                (aref img (+ o 2)) (aref r i))))
      (jpeg:encode-image path img 3 h w))
    (values w h)))

(defun run-bench (&key (w 1024) (h 768) (reps 3) (max-colors 256))
  (format t "~&=== jpeg-sixel benchmark: source ~dx~d (~d px), ~d colors, ~d reps ===~%"
          w h (* w h) max-colors reps)
  (multiple-value-bind (r g b) (gen-image w h)
    (let* ((npix (* w h)))
      ;; --- stage: quantization ---
      (bench ("median-cut" reps)
        (jpeg-sixel::median-cut r g b npix max-colors))
      (multiple-value-bind (palette index)
          (jpeg-sixel::median-cut r g b npix max-colors)
        ;; --- stage: pixel mapping (no dither / dither) ---
        (bench ("map-pixels plain" reps)
          (jpeg-sixel::map-pixels r g b w h palette :dither nil))
        (bench ("map-pixels dither" reps)
          (jpeg-sixel::map-pixels r g b w h palette :dither t))
        ;; --- stage: emission ---
        (let ((out nil))
          (setf out (bench ("emit-sixel" reps)
                      (jpeg-sixel::emit-sixel palette index w h)))
          (format t "  ~28a  ~d bytes~%" "  (sixel size)" (length out)))
        ;; --- stage: downscale to 640-wide ---
        (let* ((tw 640) (th (floor (* h tw) w)))
          (bench ((format nil "box-downscale ->~dx~d" tw th) reps)
            (jpeg-sixel::box-downscale r g b w h tw th))))))
  ;; --- end to end from a real JPEG file ---
  (let ((path (merge-pathnames "jpeg-sixel-bench.jpg" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (write-bench-jpeg path w h)
           (format t "  --- end-to-end (decode + pipeline, from JPEG file) ---~%")
           (bench ("decode-image" reps) (jpeg:decode-image path))
           (bench ("jpeg->sixel dither=nil" reps)
             (jpeg-sixel:jpeg->sixel path :dither nil :max-colors max-colors))
           (bench ("jpeg->sixel dither=t" reps)
             (jpeg-sixel:jpeg->sixel path :dither t :max-colors max-colors)))
      (ignore-errors (delete-file path))))
  (values))
