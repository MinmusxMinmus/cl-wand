(defpackage #:cl-wand
  (:use #:cl)
  (:export #:-><>))
(in-package :cl-wand)

(defvar *%available-modes%* '(> >> <> <>>))

(defmacro -><> (initial-form &rest forms)
  "Like -<>, but if any of >, >>, <>, or <>> are located inside FORMS,
then the arrow changes behavior to match the symbol."
  `(-<?> <> ,initial-form ,@forms))

(defmacro -<?> (mode initial-form &rest forms)
  "Behaves as any of ->, ->>, -<>, or -<>> depending on MODE."
  (if (not forms)
      initial-form
      (let ((form (car forms)))
        (if (member form *%available-modes%*)
            `(-<?> ,form ,initial-form ,@(cdr forms))
            (let ((f (macroexpand `(insert-into ,initial-form ,(car forms) ,mode))))
              `(-<?> ,mode ,f ,@(cdr forms)))))))

(defmacro insert-into (thing form mode)
  "Insert THING into FORM based on MODE."
  (when (listp form)
    (case mode
      (> (insert-second thing form))
      (>> (insert-last thing form))
      (<> (insert-in-diamond thing form #'insert-second))
      (<>> (insert-in-diamond thing form #'insert-last))
      (t nil))))

(defun insert-last (thing form)
  "Inserts THING as the last element in FORM."
  (append form (list thing)))

(defun insert-second (thing form)
  "Inserts THING as the second element in FORM."
  (if (endp form)
      (cons thing (cdr form))
      (list* (car form) thing (cdr form))))

(defun insert-in-diamond (thing form fallback)
  "Replaces all instances of <> in FORM's top elements with THING.  Uses
FALLBACK if no <> are present."
  (let ((count (count-if #'diamondp form)))
    (if (= count 0)
        (funcall fallback thing form)
        (substitute-if thing #'diamondp form))))

(defun diamondp (thing)
  "Check if THING is <>"
  (equal thing '<>))
