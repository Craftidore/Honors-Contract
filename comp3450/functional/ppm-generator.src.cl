;;;; PPM Gradient Generator

;;; globals

(defvar *version* '0.0.1)
(defvar *help-info* "ppm-generator.cl [y_size [x_size [filename]]]")
(defvar *start-color* '(0 255 0)); I'm too lazy to handle inputting custom colors
(defvar *end-color* '(255 0 0))

;;; handling meta flags (--version & --help)

(defun check-arg (args argcheck func)
  "!SideEffects! Returning first arg == argcheck, running `func` if yes"
  (defvar result (string-equal (car args) argcheck))
  (if result
    (func))
  result)

(defun print-version()
  "!SideEffects! prints version info to standard out"
  (format t "ppm-generator by Craftidore~%Version: ~a~%" *version*))

(defun print-help-info ()
  "!SideEffects! prints version info & help info to standard out"
  (print-version)
  (format t "~a" *help-info*))

(defun check-meta-flags()
  "!SideEffects! Checks for --help & --version; returns true if either flag is present."
  (and
    (check-arg *args* "--version" #'print-version)
    (check-arg *args* "--help" #'print-help-info)))

;;; handling program args

(defun read-int (args)
  "Returns arg's integer"
  (parse-integer (car *args*)))


(defun check-for-extension(filename ext)
  "Returns whether filename ends in ext"
  (string-equal (substring (reverse filename) 0 (length ext)) (reverse ext)))

(defun add-extension-if-not-present(filename ext)
  "Returns filename with ext at the end, if ext wasn't there already"
  (defparameter result-filename filename)
  (if (not (check-for-extension filename ext))
    (setf result-filename (concatenate 'string filename ext)))
  result-filename)

(defun handle-arguments(default-height default-width default-filename)
  "Returns (list height width filename), using default values if command line args aren't given"
  (defparameter height default-height)
  (defparameter width default-width)
  (defparameter filename default-filename)
  (defparameter args *args*)
  (if args
    (progn (setf height (read-int args)) (setf args (cdr args))))
    (if args
      (progn (setf width (read-int args)) (setf args (cdr args))))
      (if args (progn
                 (setf filename (car args))
                 (setf filename (add-extension-if-not-present filename ".ppm"))))
  (return-from handle-arguments (list height width filename)))

;;; creating gradient

;; NOTE: In the code below 'percent' refers to a float between 0 and 1 inclusive, not a value between 0 and 100.
;; I *do not* care.

(defun colordiff(color1 color2)
  "Returns list of lists of starting color vals & the difference between the two colors"
  (if (and color1 color2)
      (return-from colordiff
        (cons (list (car color1) (- (car color2) (car color1)))
              (colordiff (cdr color1) (cdr color2))))
      (return-from colordiff '())))

(defun generate-list-of-floats(len)
  "Generates list of floats from 0-1, inclusive, based on the size of the list"
  (defvar final-list '())
  (loop for i from 1 to len do
    (push (coerce (/ (- len i) len) 'float) final-list))
  (return-from generate-list-of-floats final-list))

(defun apply-val-diff(percent start-val diff)
  "Applies a color val & diff pair to a percent"
  (floor (+ start-val (* percent diff))))

(defun apply-color-diff(percent color1 color2)
  "Converts percent to a color that percent between color1 and color2"
  (mapcar
    (lambda (start-val-and-diff)
      (apply-val-diff percent (car start-val-and-diff) (cadr start-val-and-diff)))
    (colordiff color1 color2)))

(defun generate-gradient(width color1 color2)
  "Generates a one-line gradient of size width, going from color1 to color2"
  (mapcar
    (lambda (percent)
      (apply-color-diff percent color1 color2))
    (generate-list-of-floats width)))

;;; main program body

(defun print-ppm (outstream 1d-gradient height)
  "!SideEffects! Prints P3 ppm to outstream"
  (format outstream "P3~%# Generated with ppm-generator~%")
  (format outstream "~a" (length 1d-gradient))
  (format outstream " ")
  (format outstream "~a" height)
  (format outstream "~%255~%") ; max color
  (loop for y from 1 to height do
    (progn
      ;; initial color shouldn't be prepended with space
      (format outstream "~a" (caar 1d-gradient))
      (dolist (color-val (cdar 1d-gradient))
        (format outstream " ")
        (format outstream "~a" color-val))
      ;; output the rest of the colors
      (dolist (color-tuple (cdr 1d-gradient))
        (dolist (color-val color-tuple) 
          (format outstream " ")
          (format outstream "~a" color-val))))
      (format outstream "~%")))

(defun main()
  "!SideEffects!"
  (defvar generate-image (not (check-meta-flags)))
  (if generate-image
    (progn
      (defvar argument-data (handle-arguments 400 800 "image.ppm"))
      (defvar height (car argument-data))
      (defvar width (cadr argument-data))
      (defvar filename (caddr argument-data))
      ;DEBUG (print height)
      ;DEBUG (print width)
      ;DEBUG (print filename)
      ; can't make an image of a negative size
      (if (<= height 0) (setf height 1))
      (if (<= width 0) (setf width 1))
      (defvar gradient (generate-gradient width *start-color* *end-color*))

      (with-open-file (file
                       filename
                       :direction :output
                       :if-exists :supersede)
        (print-ppm file gradient height)))))

(main)

