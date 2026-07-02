;;;; test.lisp — smoke tests for jpeg-sixel

(defpackage :jpeg-sixel-test
  (:use :cl)
  (:export #:run-tests))
(in-package :jpeg-sixel-test)

(defvar *failures* 0)

(defmacro check (form &optional (msg nil))
  `(if ,form
       (format t "  ok   ~a~%" (or ,msg ',form))
       (progn (incf *failures*)
              (format t "  FAIL ~a~%" (or ,msg ',form)))))

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

(defun raster-dims (sixel)
  "Pull (values w h) from the sixel raster-attribute header, or NIL."
  (let* ((q (position #\q sixel))
         (semi (and q (position #\; sixel :start q))))
    (when semi
      ;; format is  \"1;1;W;H  after the q
      (let* ((s (subseq sixel (1+ q)))
             (parts (loop with start = 0
                          for sep = (position #\; s :start start)
                          for tok = (subseq s start sep)
                          collect tok
                          while sep do (setf start (1+ sep))
                          until (> start (length s)))))
        (declare (ignore parts))
        ;; simpler: regex-free scan of the four numbers after the opening quote
        (let ((qpos (position #\" s)))
          (when qpos
            (let ((nums '()) (start (1+ qpos)) (n 0))
              (loop while (and (< start (length s)) (< n 4)) do
                (multiple-value-bind (val end)
                    (parse-integer s :start start :junk-allowed t)
                  (unless val (return))
                  (push val nums) (incf n)
                  (setf start (if (and (< end (length s))
                                       (char= (char s end) #\;))
                                  (1+ end) end))
                  (when (and (< start (length s))
                             (not (digit-char-p (char s start))))
                    (unless (char= (char s (1- start)) #\;) (return)))))
              (let ((r (nreverse nums)))
                (when (>= (length r) 4)
                  (values (third r) (fourth r)))))))))))

(defun run-tests ()
  (setf *failures* 0)
  (let ((tmp (merge-pathnames "jpeg-sixel-test.jpg"
                              (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (multiple-value-bind (w h) (make-test-jpeg tmp)
             (format t "~&Encoding tests (~dx~d fixture):~%" w h)
             ;; 1. basic envelope
             (let ((s (jpeg-sixel:jpeg->sixel tmp :max-colors 64 :dither nil)))
               (check (and (char= (char s 0) #\Escape) (char= (char s 1) #\P))
                      "starts with DCS (ESC P)")
               (check (and (char= (char s (- (length s) 2)) #\Escape)
                           (char= (char s (1- (length s))) #\\))
                      "ends with ST (ESC backslash)")
               (multiple-value-bind (rw rh) (raster-dims s)
                 (check (and (eql rw w) (eql rh h))
                        "raster dims match source")))
             ;; 2. dithered path runs and stays well-formed
             (let ((s (jpeg-sixel:jpeg->sixel tmp :max-colors 16 :dither t)))
               (check (and (char= (char s 0) #\Escape)
                           (char= (char s (1- (length s))) #\\))
                      "dithered output well-formed"))
             ;; 3. downscale-to-fit
             (let ((s (jpeg-sixel:jpeg->sixel tmp :max-width 48 :dither nil)))
               (multiple-value-bind (rw rh) (raster-dims s)
                 (check (and rw (<= rw 48) (< rw w))
                        "max-width downscales")
                 (check (and rh (= rh (floor (* h rw) w)))
                        "downscale preserves aspect")))
             ;; 4. probe is safe with no tty (batch): must return NIL NIL, not hang
             (check (equal '(nil nil)
                           (multiple-value-list (jpeg-sixel:query-cell-size
                                                 :timeout-decisec 2)))
                    "query-cell-size returns NIL NIL with no tty")
             ;; 5. columns-for-width falls back cleanly
             (check (= 120 (jpeg-sixel:columns-for-width 1200 :default-cell-w 10))
                    "columns-for-width fallback")
             ;; 6. sixel-supported-p is safe in batch (no tty) -> :unknown
             (check (eq :unknown (jpeg-sixel:sixel-supported-p :timeout-decisec 2))
                    "sixel-supported-p returns :unknown with no tty")
             ;; 7. DA parser recognizes / rejects the sixel feature code
             (check (member 4 (jpeg-sixel::%parse-da-features
                               (format nil "~c[?62;4;6;22c" #\Escape)))
                    "DA parser finds sixel code 4")
             (check (not (member 4 (jpeg-sixel::%parse-da-features
                                    (format nil "~c[?62;22c" #\Escape))))
                    "DA parser rejects when 4 absent")))
      (ignore-errors (delete-file tmp)))
    (format t "~&~[All tests passed.~:;~:*~d failure(s).~]~%" *failures*)
    (when (> *failures* 0)
      (error "jpeg-sixel tests failed: ~d" *failures*))
    t))
