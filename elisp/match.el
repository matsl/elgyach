;;; match.el --- symbolic matching

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
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

;; This source is based on the symbolic pattern matching algorithm in
;; "Common LISPcraft", Robert Wilensky, ISBN: 0-393-95544-3, 1986.
;; The source here has modifications for Emacs Lisp

;; (set-macro-character #\?
;;   #'(lambda (stream char)
;;       (list '*var* (read stream t nil t))))

;; No reader macros in Emacs Lisp.  In Emacs Lisp patterns, ?x should
;; be replaced by '(*var* x).  
;;
;; Examples:
;;
;;   (match '(a b c) '(a b c)) = t, nil
;;
;;   (match '(:a :href (*var* x) (:img :src (*var* y))) 
;;          '(:a :href "http://www.example.com" (:img :src "foo"))) 
;;     => t, (((*var* y) "foo") ((*var* x) "http://www.example.com"))

(require 'cl)

(defun match (pattern1 pattern2)
  "Match PATTERN1 and PATTERN2.  Bind match variables.  Two
values are returned.  The first value is non-nil if the
patterns match.  The second value a list of variable names and
values."
  (match-with-bindings pattern1 pattern2 nil))

(defun match-with-bindings (a b bindings)
  (cond ((pattern-var-p a)
	 (variable-match a b bindings))
	((pattern-var-p b)
	 (variable-match b a bindings))
	((atom a)
	 (when (eq a b)
	   (values t bindings)))
	(t (multiple-value-bind
		 (flag carbindings)
	       (match-with-bindings (car a) (car b) bindings)
	     (when flag
	       (match-with-bindings (cdr a) (cdr b) carbindings))))))

(defun variable-match (pattern-var item bindings)
  (if (equal pattern-var item)
      (values t bindings)
      (let ((var-binding (get-binding pattern-var bindings)))
	(cond (var-binding
	       (match-with-bindings var-binding item bindings))
	      ((not (contained-in pattern-var item bindings))
	       (values t (add-binding pattern-var item bindings)))))))

(defun contained-in (pattern-var item bindings)
  (cond ((atom item) nil)
	((pattern-var-p item)
	 (or (equal pattern-var item)
	     (contained-in pattern-var (get-binding item bindings) bindings)))
	(t (or (contained-in pattern-var (car item) bindings)
	       (contained-in pattern-var (cdr item) bindings)))))

(defun add-binding (pattern-var item bindings)
  (cons (list pattern-var item) bindings))

(defun pattern-var-p (item)
  (and (listp item) (eq '*var* (car item))))

(defun get-binding (pattern-var bindings)
  (cadr (assoc* pattern-var bindings :test #'equal)))

(provide 'match)

;;; match.lisp ends here
