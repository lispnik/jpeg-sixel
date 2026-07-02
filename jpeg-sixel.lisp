(in-package :jpeg-sixel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize* '(optimize (speed 3) (safety 1) (debug 0))))

;;; ---------------------------------------------------------------------------
;;; Median-cut quantization.
;;; Input: pixels as three parallel simple-arrays r/g/b (unsigned-byte 8).
;;; Output: (values palette index-array) where palette is a vector of
;;;         (r g b) lists and index-array maps pixel i -> palette slot.
;;; ---------------------------------------------------------------------------

(defstruct (vbox (:constructor make-vbox (indices)))
  indices)                              ; vector of pixel indices in this box

(defun channel-range (idxs r g b)
  "Return (values widest-channel rmin rmax gmin gmax bmin bmax)."
  (declare #.*optimize*
           (type (simple-array fixnum (*)) idxs)
           (type (simple-array (unsigned-byte 8) (*)) r g b))
  (let ((rmn 255) (rmx 0) (gmn 255) (gmx 0) (bmn 255) (bmx 0))
    (declare (type (unsigned-byte 8) rmn rmx gmn gmx bmn bmx))
    (loop for i across idxs do
      (let ((rv (aref r i)) (gv (aref g i)) (bv (aref b i)))
        (when (< rv rmn) (setf rmn rv)) (when (> rv rmx) (setf rmx rv))
        (when (< gv gmn) (setf gmn gv)) (when (> gv gmx) (setf gmx gv))
        (when (< bv bmn) (setf bmn bv)) (when (> bv bmx) (setf bmx bv))))
    (let ((dr (- rmx rmn)) (dg (- gmx gmn)) (db (- bmx bmn)))
      (values (cond ((and (>= dr dg) (>= dr db)) 0)
                    ((>= dg db) 1)
                    (t 2))
              rmn rmx gmn gmx bmn bmx))))

(defun split-indices (idxs chan mid)
  "Partition IDXS into (values lo hi): LO holds the MID indices with the
   smallest CHAN value, HI the rest. Since CHAN is 8-bit we median-split with a
   256-bucket counting sort (O(m)) instead of a comparison sort — ties at the
   pivot value are apportioned so LO ends up exactly MID long, matching a
   sort-then-split-at-MID. Returns fresh (simple-array fixnum) subvectors."
  (declare #.*optimize*
           (type (simple-array fixnum (*)) idxs)
           (type (simple-array (unsigned-byte 8) (*)) chan)
           (type fixnum mid))
  (let ((hist (make-array 256 :element-type 'fixnum :initial-element 0))
        (m (length idxs)))
    (declare (type (simple-array fixnum (256)) hist) (type fixnum m))
    (loop for i across idxs do (incf (aref hist (aref chan i))))
    ;; pivot = value where the MID-th (0-based) element falls; BELOW = count of
    ;; strictly smaller elements.
    (let ((cum 0) (pivot 255) (below 0))
      (declare (type fixnum cum pivot below))
      (block find
        (dotimes (v 256)
          (declare (type fixnum v))
          (let ((next (+ cum (the fixnum (aref hist v)))))
            (declare (type fixnum next))
            (when (> next mid) (setf pivot v below cum) (return-from find))
            (setf cum next))))
      (let ((lo (make-array mid :element-type 'fixnum))
            (hi (make-array (- m mid) :element-type 'fixnum))
            (li 0) (hj 0) (pivot-to-lo (- mid below)))
        (declare (type (simple-array fixnum (*)) lo hi)
                 (type fixnum li hj pivot-to-lo))
        (loop for i across idxs do
          (let ((v (aref chan i)))
            (declare (type fixnum v))
            (cond ((< v pivot) (setf (aref lo li) i) (incf li))
                  ((> v pivot) (setf (aref hi hj) i) (incf hj))
                  ((> pivot-to-lo 0)
                   (setf (aref lo li) i) (incf li) (decf pivot-to-lo))
                  (t (setf (aref hi hj) i) (incf hj)))))
        (values lo hi)))))

(defun median-cut (r g b npix ncolors)
  "Quantize to at most NCOLORS. Returns (values palette-vector index-array)."
  (declare (type (simple-array (unsigned-byte 8) (*)) r g b)
           (type fixnum npix ncolors))
  (let* ((all (make-array npix :element-type 'fixnum)))
    (dotimes (i npix) (setf (aref all i) i))
    (let ((boxes (list (make-vbox all))))
      ;; Split the widest box until we have enough, or can't split further.
      (loop while (< (length boxes) ncolors) do
        ;; pick box with most pixels that still has >1 pixel
        (let ((target (reduce (lambda (a x)
                                (if (and (> (length (vbox-indices x)) 1)
                                         (or (null a)
                                             (> (length (vbox-indices x))
                                                (length (vbox-indices a)))))
                                    x a))
                              boxes :initial-value nil)))
          (unless target (return))
          (let ((idxs (vbox-indices target)))
            (multiple-value-bind (ch) (channel-range idxs r g b)
              (let ((chan (ecase ch (0 r) (1 g) (2 b)))
                    (mid (floor (length idxs) 2)))
                (multiple-value-bind (lo hi) (split-indices idxs chan mid)
                  (setf boxes (substitute-if (make-vbox lo)
                                             (lambda (x) (eq x target)) boxes))
                  (push (make-vbox hi) boxes)))))))
      ;; Build palette = average color of each box; assign indices.
      (let ((palette (make-array (length boxes)))
            (index (make-array npix :element-type '(unsigned-byte 8))))
        (loop for box in boxes for slot fixnum from 0 do
          (let ((idxs (vbox-indices box))
                (sr 0) (sg 0) (sb 0))
            (declare (type fixnum sr sg sb))
            (loop for i across idxs do
              (incf sr (aref r i)) (incf sg (aref g i)) (incf sb (aref b i)))
            (let ((n (max 1 (length idxs))))
              (setf (aref palette slot)
                    (list (round sr n) (round sg n) (round sb n))))
            (loop for i across idxs do (setf (aref index i) slot))))
        (values palette index)))))

;;; ---------------------------------------------------------------------------
;;; Downscale-to-fit (box / area averaging).
;;; Only ever shrinks; if the source already fits, the caller skips this.
;;; ---------------------------------------------------------------------------

(defun fit-dimensions (sw sh max-w max-h)
  "Return (values tw th) — largest size <= source that fits max-w X max-h,
   preserving aspect ratio. NIL for a bound means unconstrained. Never upscales."
  (declare (type fixnum sw sh))
  (let ((scale 1.0))
    (when (and max-w (> sw max-w)) (setf scale (min scale (/ max-w sw))))
    (when (and max-h (> sh max-h)) (setf scale (min scale (/ max-h sh))))
    (if (>= scale 1.0)
        (values sw sh)
        (values (max 1 (floor (* sw scale)))
                (max 1 (floor (* sh scale)))))))

(defun box-downscale (r g b sw sh tw th)
  "Area-average R/G/B (each W*H (unsigned-byte 8)) from SW x SH down to TW x TH.
   Returns (values nr ng nb). Assumes tw<=sw, th<=sh."
  (declare #.*optimize*
           (type (simple-array (unsigned-byte 8) (*)) r g b)
           (type fixnum sw sh tw th))
  (let ((nr (make-array (* tw th) :element-type '(unsigned-byte 8)))
        (ng (make-array (* tw th) :element-type '(unsigned-byte 8)))
        (nb (make-array (* tw th) :element-type '(unsigned-byte 8))))
    (dotimes (ty th)
      (let ((sy0 (floor (* ty sh) th))
            (sy1 (max (1+ (floor (* ty sh) th)) (floor (* (1+ ty) sh) th))))
        (declare (type fixnum sy0 sy1))
        (dotimes (tx tw)
          (let ((sx0 (floor (* tx sw) tw))
                (sx1 (max (1+ (floor (* tx sw) tw)) (floor (* (1+ tx) sw) tw)))
                (sr 0) (sg 0) (sb 0) (cnt 0))
            (declare (type fixnum sx0 sx1 sr sg sb cnt))
            (loop for sy fixnum from sy0 below (min sy1 sh) do
              (let ((row (* sy sw)))
                (loop for sx fixnum from sx0 below (min sx1 sw) do
                  (let ((i (+ row sx)))
                    (incf sr (aref r i)) (incf sg (aref g i)) (incf sb (aref b i))
                    (incf cnt)))))
            (let ((o (+ (* ty tw) tx))
                  (n (max 1 cnt)))
              (setf (aref nr o) (round sr n)
                    (aref ng o) (round sg n)
                    (aref nb o) (round sb n)))))))
    (values nr ng nb)))

;;; ---------------------------------------------------------------------------
;;; Nearest-color lookup with a 32^3 cache, and Floyd-Steinberg dithering.
;;;
;;; The palette arrives as a vector of (r g b) lists. For fast repeated lookup
;;; we (a) unpack it into three fixnum arrays, and (b) memoize nearest-slot
;;; results in a 32x32x32 LUT keyed on the top 5 bits of each channel.
;;; ---------------------------------------------------------------------------

(defun palette->arrays (palette)
  "Return (values pr pg pb) as simple fixnum arrays for channel r/g/b."
  (let* ((n (length palette))
         (pr (make-array n :element-type 'fixnum))
         (pg (make-array n :element-type 'fixnum))
         (pb (make-array n :element-type 'fixnum)))
    (loop for entry across palette for i fixnum from 0 do
      (destructuring-bind (rr gg bb) entry
        (setf (aref pr i) rr (aref pg i) gg (aref pb i) bb)))
    (values pr pg pb)))

(declaim (inline nearest-slot))
(defun nearest-slot (rv gv bv pr pg pb ncolors)
  "Exhaustive nearest palette slot for color (RV GV BV) by squared distance."
  (declare #.*optimize*
           (type fixnum rv gv bv ncolors)
           (type (simple-array fixnum (*)) pr pg pb))
  (let ((best 0) (bestd most-positive-fixnum))
    (declare (type fixnum best bestd))
    (dotimes (i ncolors)
      (let* ((dr (- rv (aref pr i)))
             (dg (- gv (aref pg i)))
             (db (- bv (aref pb i)))
             (d (the fixnum (+ (* dr dr) (* dg dg) (* db db)))))
        (declare (type fixnum dr dg db d))
        (when (< d bestd) (setf bestd d best i))))
    best))

(defmacro lut-key (rv gv bv)
  "Index into a 32^3 LUT from 8-bit channels (top 5 bits each)."
  `(the fixnum (+ (ash (ash ,rv -3) 10)
                  (ash (ash ,gv -3) 5)
                  (ash ,bv -3))))

(declaim (inline clamp8))
(defun clamp8 (x)
  (declare (type fixnum x))
  (cond ((< x 0) 0) ((> x 255) 255) (t x)))

(defun map-pixels (r g b w h palette &key dither)
  "Map each pixel to a palette slot. When DITHER, apply Floyd-Steinberg.
   Returns an (unsigned-byte 8) index array of length W*H.
   Note: dithering modifies working copies of r/g/b (error diffusion), not
   the originals, so callers may reuse the source arrays.

   Nearest-slot lookups are memoized in a 32^3 LUT keyed on the top 5 bits of
   each channel; the lookup is a local INLINE flet (no per-pixel funcall) and
   reuses the unpacked PR/PG/PB fixnum arrays both to search and to read back
   the chosen color's components (no per-pixel consing)."
  (declare #.*optimize*
           (type (simple-array (unsigned-byte 8) (*)) r g b)
           (type fixnum w h))
  (multiple-value-bind (pr pg pb) (palette->arrays palette)
    (let* ((ncolors (length palette))
           (npix (* w h))
           (lut (make-array (* 32 32 32) :element-type 'fixnum :initial-element -1))
           (index (make-array npix :element-type '(unsigned-byte 8))))
      (declare (type (simple-array fixnum (*)) pr pg pb lut)
               (type fixnum ncolors npix))
      (flet ((nearest (rv gv bv)
               (declare (type fixnum rv gv bv))
               (let* ((k (lut-key rv gv bv))
                      (cached (aref lut k)))
                 (declare (type fixnum k cached))
                 (if (>= cached 0)
                     cached
                     (setf (aref lut k)
                           (nearest-slot rv gv bv pr pg pb ncolors))))))
        (declare (inline nearest))
        (if (not dither)
            (dotimes (i npix)
              (setf (aref index i)
                    (nearest (aref r i) (aref g i) (aref b i))))
            ;; Floyd-Steinberg: keep signed error-carrying working rows.
            (let ((wr (make-array npix :element-type 'fixnum))
                  (wg (make-array npix :element-type 'fixnum))
                  (wb (make-array npix :element-type 'fixnum)))
              (declare (type (simple-array fixnum (*)) wr wg wb))
              (dotimes (i npix)
                (setf (aref wr i) (aref r i)
                      (aref wg i) (aref g i)
                      (aref wb i) (aref b i)))
              (flet ((spread (i er eg eb num)
                       (declare (type fixnum i er eg eb num))
                       (incf (aref wr i) (ash (* er num) -4))
                       (incf (aref wg i) (ash (* eg num) -4))
                       (incf (aref wb i) (ash (* eb num) -4))))
                (declare (inline spread))
                (dotimes (y h)
                  (let ((left-to-right (evenp y))) ; serpentine scan
                    (labels ((do-col (x)
                               (declare (type fixnum x))
                               (let* ((i (+ (* y w) x))
                                      (rv (clamp8 (aref wr i)))
                                      (gv (clamp8 (aref wg i)))
                                      (bv (clamp8 (aref wb i)))
                                      (slot (nearest rv gv bv))
                                      (er (- rv (aref pr slot)))
                                      (eg (- gv (aref pg slot)))
                                      (eb (- bv (aref pb slot)))
                                      (dir (if left-to-right 1 -1)))
                                 (declare (type fixnum er eg eb dir))
                                 (setf (aref index i) slot)
                                 ;; right (7), below-left(3), below(5), below-right(1)
                                 (let ((xr (+ x dir)))
                                   (when (and (>= xr 0) (< xr w))
                                     (spread (+ i dir) er eg eb 7)))
                                 (when (< (1+ y) h)
                                   (let ((bi (+ i w)))
                                     (spread bi er eg eb 5)
                                     (let ((xbl (- x dir)))
                                       (when (and (>= xbl 0) (< xbl w))
                                         (spread (- bi dir) er eg eb 3)))
                                     (let ((xbr (+ x dir)))
                                       (when (and (>= xbr 0) (< xbr w))
                                         (spread (+ bi dir) er eg eb 1))))))))
                      (if left-to-right
                          (loop for x fixnum from 0 below w do (do-col x))
                          (loop for x fixnum from (1- w) downto 0 do (do-col x))))))))))
      index)))

;;; ---------------------------------------------------------------------------
;;; Sixel emission.
;;; ---------------------------------------------------------------------------

(defun emit-run (out ch count)
  "Emit CH repeated COUNT times, using RLE (!) when it pays off."
  (declare (type fixnum count))
  (cond ((<= count 0))
        ((<= count 3) (dotimes (_ count) (write-char ch out)))
        (t (write-char #\! out) (princ count out) (write-char ch out))))

(defun emit-sixel (palette index w h)
  "Encode INDEX (an (unsigned-byte 8) palette-slot array of length W*H) plus
   PALETTE (vector of (r g b) lists) as a sixel string. Pure function of its
   arguments — no decoding or scaling."
  (declare #.*optimize*
           (type (simple-array (unsigned-byte 8) (*)) index)
           (type fixnum w h))
  (let* ((ncolors (length palette))
         ;; Reused per-color column masks: MASKS[color*w + col] holds the 6-bit
         ;; sixel pattern for that color in the current band. Filled in one
         ;; w*6 pass (rather than rescanning the band once per color), and each
         ;; entry is zeroed again as it is emitted, so it stays all-zero between
         ;; bands without a separate clear pass.
         (masks (make-array (* ncolors w) :element-type '(unsigned-byte 8)
                                          :initial-element 0))
         (used (make-array ncolors :element-type 'bit :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 8) (*)) masks)
             (type fixnum ncolors))
    (with-output-to-string (out)
      ;; Device Control String intro + raster attributes.
      (format out "~cPq" #\Escape)
      (format out "\"1;1;~d;~d" w h)
      ;; Palette registers: sixel wants 0..100 scaled RGB, format 2 = RGB.
      (loop for entry across palette for n fixnum from 0 do
        (destructuring-bind (rr gg bb) entry
          (format out "#~d;2;~d;~d;~d" n
                  (round (* rr 100) 255)
                  (round (* gg 100) 255)
                  (round (* bb 100) 255))))
      ;; Bands of 6 rows.
      (loop for band-top fixnum from 0 below h by 6 do
        (let ((band-h (min 6 (- h band-top))))
          (declare (type (integer 1 6) band-h))
          (fill used 0)
          ;; Single pass: OR each pixel's row-bit into its color's column mask.
          (dotimes (row band-h)
            (let ((base (the fixnum (* (+ band-top row) w)))
                  (bit (the (unsigned-byte 6) (ash 1 row))))
              (dotimes (col w)
                (let* ((color (aref index (+ base col)))
                       (mi (the fixnum (+ (the fixnum (* color w)) col))))
                  (setf (sbit used color) 1)
                  (setf (aref masks mi) (logior (aref masks mi) bit))))))
          ;; One output pass per used color; clear its masks as we read them.
          (let ((first-pass t))
            (dotimes (color ncolors)
              (when (= 1 (sbit used color))
                (unless first-pass (write-char #\$ out)) ; CR between passes
                (setf first-pass nil)
                (format out "#~d" color)
                (let ((coff (the fixnum (* color w)))
                      (run-char nil) (run-len 0))
                  (declare (type fixnum run-len))
                  (dotimes (col w)
                    (let* ((mi (+ coff col))
                           (mask (aref masks mi))
                           (c (code-char (+ 63 mask))))
                      (setf (aref masks mi) 0) ; reset for the next band
                      (if (eql c run-char)
                          (incf run-len)
                          (progn (when run-char (emit-run out run-char run-len))
                                 (setf run-char c run-len 1)))))
                  (when run-char (emit-run out run-char run-len))))))
          (write-char #\- out)))            ; newline: next band
      ;; String Terminator.
      (format out "~c\\" #\Escape))))

(defun jpeg->sixel (filename &key (max-colors 256) (dither t)
                             max-width max-height cols)
  "Decode JPEG FILENAME and return a sixel-encoded string.
   MAX-COLORS caps the palette (<=256). DITHER enables Floyd-Steinberg.
   Downscale-to-fit (never upscales), pick at most one convention:
     :MAX-WIDTH / :MAX-HEIGHT  bounds in pixels (either may be NIL)
     :COLS                     fit to N terminal character columns; assumes a
                               ~1:2 cell aspect so height is bounded to keep
                               the image looking right in a text grid."
  (multiple-value-bind (buf h w ncomp) (jpeg:decode-image filename)
    (declare (type (simple-array (unsigned-byte 8) (*)) buf)
             (type fixnum h w ncomp))
    (let* ((npix (* h w))
           (r (make-array npix :element-type '(unsigned-byte 8)))
           (g (make-array npix :element-type '(unsigned-byte 8)))
           (b (make-array npix :element-type '(unsigned-byte 8))))
      ;; Deinterleave. cl-jpeg gives BGR for 3-component, single channel for gray.
      (if (= ncomp 1)
          (dotimes (i npix)
            (let ((v (aref buf i)))
              (setf (aref r i) v (aref g i) v (aref b i) v)))
          (dotimes (i npix)
            (let ((o (* i ncomp)))
              (setf (aref b i) (aref buf o)
                    (aref g i) (aref buf (+ o 1))
                    (aref r i) (aref buf (+ o 2))))))
      ;; Downscale-to-fit before quantizing. :COLS is a terminal-cell convenience:
      ;; N columns of ~ (cell-w) px, and to preserve shape in the ~1:2 cell grid
      ;; we cap height proportionally. Explicit :MAX-WIDTH/:MAX-HEIGHT override.
      ;; N columns wide. We probe the terminal for its actual pixel cell size;
      ;; if that fails (no tty / unsupported) we fall back to a nominal 10px.
      ;; Explicit :MAX-WIDTH/:MAX-HEIGHT still override.
      (let ((mw max-width) (mh max-height))
        (when cols
          (let ((cell-w (or (and (fboundp 'query-cell-size)
                                 (nth-value 0 (query-cell-size)))
                            10)))
            (setf mw (or mw (* cols cell-w)))))
        (multiple-value-bind (tw th) (fit-dimensions w h mw mh)
          (declare (type fixnum tw th))
          (when (or (< tw w) (< th h))
            (multiple-value-bind (nr ng nb) (box-downscale r g b w h tw th)
              (setf r nr g ng b nb w tw h th npix (* tw th))))))
      (multiple-value-bind (palette raw-index)
          (median-cut r g b npix (min max-colors 256))
        ;; median-cut gives us a palette; when dithering we discard its cheap
        ;; index and recompute with error diffusion. Non-dither reuses raw-index.
        (let ((index (if dither
                         (map-pixels r g b w h palette :dither t)
                         raw-index)))
          (emit-sixel palette index w h))))))

(defun write-jpeg-sixel (filename &optional (stream *standard-output*)
                         &rest keys &key &allow-other-keys)
  "Decode FILENAME and write its sixel representation to STREAM.
   Extra keywords (:max-colors, :dither) are forwarded to JPEG->SIXEL."
  (write-string (apply #'jpeg->sixel filename keys) stream)
  (values))
