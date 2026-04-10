(defpackage #:cl-wand
  (:use #:cl)
  (:export #:-><>))
(in-package :cl-wand)

(defvar *modes* '(">" ">>" "<>" "<>>")
  "Available wand modes.")

(defun mode-p (thing)
  "Check if THING is a wand mode."
  (if (and (symbolp thing)
           (member (symbol-name thing) *modes* :test #'equal))
      t
      nil))

(defun mode= (a b)
  "Check if A and B are the same wand mode."
  (when (and (mode-p a)
             (mode-p b))
    (string= (symbol-name a) (symbol-name b))))

(defun diamond-p (thing)
  "Check if THING is <>"
  (and (symbolp thing)
       (string= "<>" (symbol-name thing))))

(defun insert-second (thing form)
  "Inserts THING as the second element in FORM."
  (if (endp form)
      (cons thing (cdr form))
      (list* (car form) thing (cdr form))))

(defun insert-last (thing form)
  "Inserts THING as the last element in FORM."
  (append form (list thing)))

(defun insert-in-diamond (thing form fallback)
  "Replaces all instances of <> in FORM's top elements with THING.  Uses
FALLBACK if no <> are present."
  (let ((count (count-if #'diamond-p form)))
    (if (= count 0)
        (funcall fallback thing form)
        (substitute-if thing #'diamond-p form))))

(defmacro -><> (initial-form &rest forms)
  "Like -<>, but if any of >, >>, <>, or <>> are located inside FORMS,
then the arrow changes behavior to match the symbol."
  (let ((mode '<>))
    (reduce #'(lambda (a b)
                (cond
                  ((mode-p a)             ; Mode and something.
                   (setq mode a)
                   b)
                  ((mode-p b)             ; Non-mode and mode.
                   (setq mode b)
                   a)
                  ((symbolp b)            ; Non-mode and symbol.
                   (list b a))
                  (t                      ; Non-mode and non-symbol.
                   (cond
                     ((mode= mode '>)
                      (insert-second a b))
                     ((mode= mode '>>)
                      (insert-last a b))
                     ((mode= mode '<>)
                      (insert-in-diamond a b #'insert-second))
                     ((mode= mode '<>>)
                      (insert-in-diamond a b #'insert-last))))))
            forms
            :initial-value initial-form)))
