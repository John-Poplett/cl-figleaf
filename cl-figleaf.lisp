;; Copyright (c) 2010-2011 John H. Poplett.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;

(in-package :cl)

(defpackage :cl-figleaf
  (:use :cl)
  (:nicknames "figleaf")
  (:documentation "A tiny code coverage tool for Common Lisp.")
  (:export
   #:run-tests
   #:tested
   #:untested
   #:with-instrument-package))

(defpackage :cl-figleaf-demo
  (:use :cl)
  (:export #:foo #:add-one))

(defpackage :cl-figleaf-demo-test
(:use :cl :cl-figleaf-demo))

(in-package :cl-figleaf)

(require 'lisp-unit)

;; Function below is based on this discussion from the comp.lang.lisp newsgroup:
;;
;; You can use SYMBOL-FUNCTION to retrieve the function associated with a symbol. 
;; A SYMBOL-FUNCTION form is a place, so you can assign a new function there. 
;; (defun instrument-function (name pre post) 
;;  (let ((old (symbol-function name))) 
;;    (setf (symbol-function name) 
;;          (lambda (&rest args) 
;;            (prog2 
;;              (funcall pre name args) 
;;              (apply old args) 
;;              (funcall post name args)))))) 
;; A better version of this would save the old function in a way that you can 
;; uninstrument it, analogous to UNTRACE. Also, the post-advice might have access 
;; to the return value, rather than just the arguments. Etc.  Everything can be 
;; improved, extended, as usual. 

(defun instrument-function (name pre post)
  "Wrap given func with pre- and post-function calls. Return a let-over-lambda
expression to restore to original."
 (let ((old (symbol-function name))) 
   (setf (symbol-function name) 
         (lambda (&rest args) 
           (prog2 
             (if pre (funcall pre name args))
             (apply old args)
             (if post (funcall post name args)))))
   (lambda () 
     (setf (symbol-function name) old))))

(defmacro with-instrument-package ((package &key pre post) &body body)
  "Wrap each function of the given package with pre and post function
calls. Call code specified in the body and restore the funcitons on exit."
  (let ((restore-list (gensym)))
  `(let ((,restore-list nil))
     (unwind-protect 
	  (loop for name being the external-symbol of ,package when (fboundp name)
	       do (push (instrument-function name ,pre ,post) ,restore-list))
	    ,@body)
       (dolist (restore-func ,restore-list)
	   (funcall restore-func)))))

(let ((funcall-counter (make-hash-table))
      (package nil))
  (defun all ()
    (loop for name being the external-symbol of package when (fboundp name) collect name))
  (defun tested ()
    "Return a list of tested functions"
    (loop for name being the hash-keys in funcall-counter collect name))
  (defun untested ()
    "Return a list of untested functions."
    (set-difference (all) (tested)))
  (defun increment-funcall-count (name)
    (cond ((null (gethash name funcall-counter)) (setf (gethash name funcall-counter) 1))
	  (t (incf (gethash name funcall-counter)))))
  (defun funcall-count (&optional name)
    (cond ((null name) (hash-table-count funcall-counter))
	  (t (cond ((null (gethash name funcall-counter)) 0)
		   (t (gethash name funcall-counter))))))
  (defun package-function-count () 
    (loop for name being the external-symbol of package when (fboundp name) 
       count it into count
       finally (return count)))
  (defun reset-funcall-counter ()
    (setf funcall-counter (make-hash-table)))
  (defun run-tests (package-under-test unit-test-package)
    "Instrument the package under test and run the tests specified in unit-test-package. Count the
number of times each exported function in the package under test is invoked. Print results at end of run."
    (setf package package-under-test)
    (reset-funcall-counter)
    (with-instrument-package (package-under-test :pre #'(lambda (name &rest args) (declare (ignore args)) (increment-funcall-count name)))
      (let ((*package* (find-package unit-test-package)))
	(lisp-unit:run-tests)))
    (format t "~%CODE COVERAGE: Functions ~A, Tested ~A, Ratio ~,2f~%" (package-function-count) (funcall-count) (/ (funcall-count) (package-function-count) .01))))

(in-package :cl-figleaf-demo)

(defun foo ()
  (format t "~A~%" "hello world!"))

(defun add-one (number)
  (1+ number))

(in-package :cl-figleaf-demo-test)

(require 'lisp-unit)

(lisp-unit:define-test add-one-test
  (lisp-unit:assert-equal 1 (add-one 0)))

(defun run-tests () 
  (cl-figleaf:run-tests :cl-figleaf-demo :cl-figleaf-demo-test))

