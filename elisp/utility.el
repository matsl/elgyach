;;; utility.el --- general utilities

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mbkennedy@austin.rr.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

;; These functions are from the Paul Graham On Lisp code maintained in
;; the Common Lisp Open Code Collection:
;; http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/onlisp/onlisp-util.lisp

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."   ; LMH
  `(let ((it ,test-form))
     (declare (ignorable it))		; LMH
     (if it ,then-form ,else-form)))


(defmacro awhen (test-form &body body)
  "Anaphoric when: use `it' in body to
   refer to result of the test-form."   ; LMH
  `(aif ,test-form
        (progn ,@body)))

(defmacro acond (&rest clauses)
  "Anaphoric cond: use `it' in consequent to refer to result of test."    ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
		 (declare (ignorable it)) ; LMH
		 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(provide 'utility)

;;; utility.el ends here
