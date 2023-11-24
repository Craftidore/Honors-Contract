;;;; PPM Gradient Generator

(defvar *version* '0.0.0_beta)
(defvar *help-info* "ppm-generator.cl [y_size [x_size [filename]]]")
(defparameter *generate-image* nil) ; if `--version` or `--help`, don't generate an image

; (format t "~a~%" (car *args*))


;; Handle --version & --help
(if *args*
  (progn
    (cond ((string-equal (car *args*) "--version")
           (format t "ppm-generator by Craftidore~%Version: ~a~%" *version*))
          ((string-equal (car *args*) "--help")
           (progn
             (format t "ppm-generator by Craftidore~%Version: ~a~%" *version*)
             (format t "~a" *help-info*))))
    (if (not (string-equal (substring (car *args*) 0 2) "--")) (setf *generate-image* t)))
  (setf *generate-image* t))

(defun handle-arguments(default-height default-width default-filename)
      (defparameter height default-height)
      (defparameter width default-width)
      (defparameter filename default-filename)
      (if *args*
        (progn
          (setf height (parse-integer (car *args*)))
          (if (cdr *args*) ;; is second argument nil?
            (progn
              (setf width (parse-integer (cadr *args*)))
              (if (caddr *args*) ; is third argument nil?
                (progn
                  (setf filename (caddr *args*))
                        (if (not (string-equal (substring (reverse filename) 0 4) (reverse ".ppm")))
                          (setf filename (concatenate 'string filename ".ppm")))))))))
  )

(defun colordiff(color1 color2)
  (if (car color1)
    (if (car color2)
      (return-from colordiff
        (cons (list (car color1) (- (car color2) (car color1)))
              (colordiff (cdr color1) (cdr color2))))))
  (if (or (not (car color1)) (not (car color2)))
    (return-from colordiff '())))

(defun generate-gradient(width color1 color2)
  (defvar *list* '())
  (loop for i from 1 to width do
        (push (coerce (/ (- width i) width) 'float) *list*))
  (return-from generate-gradient (mapcar (lambda (x) 
            (mapcar (lambda (val-and-diff) 
                (floor (+ (car val-and-diff) (* x (cadr val-and-diff)))))
              (colordiff color1 color2)))
          *list*)))



;DEBUG (print *generate-image*)

(if *generate-image*
  (progn
      (defvar *argument-data* (handle-arguments 400 800 "image.ppm"))
      (defvar height (car *argument-data*))
      (defvar width (cadr *argument-data*))
      (defvar filename (caddr *argument-data*))
      ;DEBUG (print height)
      ;DEBUG (print width)
      ;DEBUG (print filename)
      ; can't make an image of a negative size
      (if (<= height 0) (setf height 1))
      (if (<= width 0) (setf width 1))
      (defvar gradient (generate-gradient width '(0 255 0) '(255 0 0)))

      (with-open-file (file
                         filename
                         :direction :output
                         :if-exists :supersede)
          (format file "P3~%# Generated with ppm-generator~%")
          (format file "~a" width file)
          (format file " " file)
          (format file "~a" height file)
          (format file "~%255~%") ; max color
          (loop for y from 1 to height do
                (progn
                    (princ (car (car gradient)) file)
                    (mapcar (lambda (color-val) (princ " " file) (princ color-val file)) (cdar gradient))
                    (mapcar (lambda (color-tuple) 
                              (mapcar (lambda (color-val) (princ " " file) (princ color-val file)) color-tuple)) (cdr gradient))
                    (format file "~%")
                ))
        )
  )
)
  
 
