;;;; terminal-probe.lisp — query a terminal for its cell size in pixels.
;;;;
;;;; Uses XTerm window-ops:  ESC [ 14 t  -> text area size in PIXELS
;;;;                         ESC [ 18 t  -> text area size in CHARACTER CELLS
;;;; Reply format:           ESC [ 4 ; height ; width t   (for 14t, pixels)
;;;;                         ESC [ 8 ; rows   ; cols  t   (for 18t, cells)
;;;;
;;;; Everything here is best-effort. If there is no tty, the terminal does not
;;;; answer, the reply is malformed, or stty is unavailable, we return NIL and
;;;; the caller uses a sane default. We NEVER block indefinitely.

(in-package :jpeg-sixel)

;;; --- raw-mode plumbing via stty (portable across SBCL builds) ---------------

(defun %run (program args &key input)
  "Run PROGRAM with ARGS, optionally feeding INPUT string to stdin.
   Returns (values stdout-string exit-code) or (values nil nil) on failure.
   stdin/stdout are wired to the terminal so stty affects the real tty."
  (handler-case
      (let ((proc (sb-ext:run-program program args
                                      :search t
                                      :input (if input
                                                 (make-string-input-stream input)
                                                 t)   ; inherit tty
                                      :output :stream
                                      :error nil
                                      :wait t)))
        (values (with-output-to-string (s)
                  (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                        while line do (write-line line s)))
                (sb-ext:process-exit-code proc)))
    (error () (values nil nil))))

(defun %stty (arg-string)
  "Invoke stty with ARG-STRING (space-separated). Returns t on success.
   stdin must be the tty, so we inherit it (:input t)."
  (handler-case
      (let ((proc (sb-ext:run-program "stty"
                                      (let ((a (list)))
                                        (dolist (tok (uiop-split arg-string) (nreverse a))
                                          (push tok a)))
                                      :search t :input t :output nil :error nil
                                      :wait t)))
        (eql 0 (sb-ext:process-exit-code proc)))
    (error () nil)))

(defun uiop-split (s)
  "Minimal whitespace split (avoid depending on UIOP being loaded)."
  (let ((out '()) (start nil))
    (loop for i from 0 below (length s)
          for c = (char s i) do
            (if (member c '(#\Space #\Tab))
                (when start (push (subseq s start i) out) (setf start nil))
                (unless start (setf start i)))
          finally (when start (push (subseq s start) out)))
    (nreverse out)))

;;; --- the actual query -------------------------------------------------------

(defun %tty-p ()
  "True if we appear to be attached to a terminal. We treat a successful
   `stty -g` (which only works on a tty) as the definitive check — this avoids
   a separate subprocess whose fds may not inherit the controlling terminal."
  (multiple-value-bind (out code) (%run "stty" '("-g"))
    (and (eql 0 code)
         out
         (> (length (string-trim '(#\Newline #\Return #\Space) out)) 0))))

(defun %read-reply (timeout-decisec &optional (terminator #\t))
  "Read a CSI reply from *standard-input* with a coarse timeout.
   Relies on stty min 0 time N having been set so reads are non-blocking-ish.
   Collects bytes until TERMINATOR or a short idle. Returns the raw string
   (including the leading ESC) or NIL."
  (let ((buf (make-string-output-stream))
        (deadline (+ (get-internal-real-time)
                     (* timeout-decisec (/ internal-time-units-per-second 10))))
        (got nil))
    (loop
      (when (> (get-internal-real-time) deadline) (return))
      (let ((c (read-char-no-hang *standard-input* nil :eof)))
        (cond ((null c)
               ;; nothing available yet; brief spin, but bounded by deadline
               (sleep 0.005))
              ((eq c :eof) (return))
              (t (setf got t)
                 (write-char c buf)
                 (when (char= c terminator) (return))))))
    (and got (get-output-stream-string buf))))

(defun %parse-csi-t (reply)
  "Parse ESC [ a ; b ; c t -> (values a b c) as integers, or NIL."
  (when (and reply (>= (length reply) 4))
    (let* ((body (subseq reply (1+ (or (position #\[ reply) 0))))
           (end (position #\t body))
           (nums (and end
                      (mapcar (lambda (x) (parse-integer x :junk-allowed t))
                              (loop with s = (subseq body 0 end)
                                    with start = 0
                                    for sep = (position #\; s :start start)
                                    collect (subseq s start sep)
                                    while sep do (setf start (1+ sep)))))))
      (when (and nums (every #'integerp nums))
        (values-list nums)))))

(defun query-cell-size (&key (timeout-decisec 3))
  "Return (values cell-width-px cell-height-px) for the current terminal, or
   NIL NIL if it can't be determined. Non-blocking beyond TIMEOUT-DECISEC
   tenths of a second. Safe to call when not attached to a terminal."
  (unless (%tty-p)
    (return-from query-cell-size (values nil nil)))
  (let ((saved (nth-value 0 (%run "stty" '("-g")))))
    (unless saved (return-from query-cell-size (values nil nil)))
    (setf saved (string-trim '(#\Newline #\Return) saved))
    (unwind-protect
         (progn
           (unless (%stty (format nil "-icanon -echo min 0 time ~d" timeout-decisec))
             (return-from query-cell-size (values nil nil)))
           (flet ((ask (code)
                    (format *standard-output* "~c[~dt" #\Escape code)
                    (finish-output *standard-output*)
                    (%parse-csi-t (%read-reply timeout-decisec))))
             (multiple-value-bind (k ph pw) (ask 14)   ; pixels: k=4,height,width
               (multiple-value-bind (k2 rows cols) (ask 18) ; cells: k=8,rows,cols
                 (declare (ignore k k2))
                 (if (and ph pw rows cols
                          (> rows 0) (> cols 0))
                     (values (floor pw cols) (floor ph rows))
                     (values nil nil))))))
      ;; always restore tty
      (when saved (%stty (format nil "~a" saved))))))

(defun columns-for-width (target-px &key (default-cell-w 10))
  "Given a desired image width in PIXELS, return how many terminal columns that
   spans, using a live cell-size probe when possible, else DEFAULT-CELL-W."
  (let ((cw (or (nth-value 0 (query-cell-size)) default-cell-w)))
    (max 1 (floor target-px cw))))

;;; --- sixel capability via Primary Device Attributes ------------------------
;;;
;;; ESC [ c  requests Primary DA. The reply is  ESC [ ? p1 ; p2 ; ... c
;;; where each pN is a feature code. Code 4 = sixel graphics.

(defun %parse-da-features (reply)
  "Parse a Primary DA reply (ESC [ ? n;n;...c) into a list of integer feature
   codes, or NIL if REPLY is not a well-formed DA response."
  (when (and reply (find #\c reply))
    (let* ((qpos (position #\? reply))
           (cpos (position #\c reply))
           (body (and qpos cpos (< qpos cpos) (subseq reply (1+ qpos) cpos))))
      (when body
        (let ((codes '()) (start 0))
          (loop
            (let ((sep (position #\; body :start start)))
              (let ((tok (subseq body start (or sep (length body)))))
                (let ((n (parse-integer tok :junk-allowed t)))
                  (when n (push n codes))))
              (if sep (setf start (1+ sep)) (return))))
          (nreverse codes))))))

(defun sixel-supported-p (&key (timeout-decisec 3))
  "Return T if the terminal's Primary Device Attributes report sixel support
   (feature code 4), NIL if it does not, and :UNKNOWN if we can't tell (no tty,
   no reply, or a malformed response). Never blocks beyond TIMEOUT-DECISEC."
  (unless (%tty-p)
    (return-from sixel-supported-p :unknown))
  (let ((saved (nth-value 0 (%run "stty" '("-g")))))
    (unless saved (return-from sixel-supported-p :unknown))
    (setf saved (string-trim '(#\Newline #\Return) saved))
    (unwind-protect
         (progn
           (unless (%stty (format nil "-icanon -echo min 0 time ~d" timeout-decisec))
             (return-from sixel-supported-p :unknown))
           (format *standard-output* "~c[c" #\Escape)
           (finish-output *standard-output*)
           (let ((features (%parse-da-features
                            (%read-reply timeout-decisec #\c))))
             (cond ((null features) :unknown)
                   ((member 4 features) t)
                   (t nil))))
      (when saved (%stty (format nil "~a" saved))))))
