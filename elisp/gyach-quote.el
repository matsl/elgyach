;;; gyach-quote.el --- ElGyach quote database interface

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@killr.ath.cx>
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

(require 'cl)

(defcustom gyach-quote-database-alist '()
  "List of cons cells representing quote database files and the
name of the database like this: (NAME . DATABASEFILE)"
  :type 'alist
  :group 'gyach)

(defun gyach-custom-QUOTE (proc argument)
  "Take a random quote from a set of quote databases.  Two
arguments in `argument' represent a database/user pair in which
the quote should be directed at a specific user.  One argument
sends a quote generally.  This command understand one or two
arguments.  The first argument is the name of the database to
draw from, the second argument (optional) is prefix text."
  (string-match "^ *\\(\\w+\\)\\( +\\(.*\\w\\) *\\)?" argument)
  (let* ((database-name (match-string 1 argument))
	 (argument (match-string 3 argument))
	 (database-info (assoc database-name gyach-quote-database-alist)))
    (if (not (null database-info))
	(let ((file (car database-info))
	      (prefix (cdr database-info)))
	  )
    
    (assoc database-name 
	

(gyach-custom-QUOTE nil " hosts ")


(defun gyach-get-quote (&optional database-name prefix)
  (when (zerop (length gyach-quote-database-alist))
    (error "No databases exist."))
  (when (null database-name)
    (setq database-name (car (elt gyach-quote-database-alist 
				  (random (length gyach-quote-database))))))
  (let ((database-file (cdr (assoc database-name gyach-quote-database-alist))))
    (with-temp-buffer 
      (insert-file database-file)
      (let* ((file-length (count-lines (point-min) (point-max)))
	     (line (random file-length)))
	(goto-char (point-min))
	(dotimes (l line)
	  (search-forward "\n"))
	(let ((bol (prog2 (beginning-of-line) (point)))
	      (eol (prog2 (end-of-line) (point))))
	(buffer-substring bol eol))))))

(add-to-list 'gyach-quote-database-alist '("hai" . "~/hais.txt"))
;; (add-to-list 'gyach-quote-database-alist '("bofh" . '("~/excuses.txt" . "BOFH says:")))
;; (gyach-get-quote "hai")

(defun gyach-custom-HAI (proc arg) (comint-send-string proc (concat (gyach-get-quote "hai") "\n\n")))
;; (defun gyach-custom-BOFH (proc arg) (comint-send-string proc (concat (gyach-get-quote "bofh") "\n\n")))
;(gyach-custom-HAI nil nil)
  
(provide 'gyach-quote)
 
;;; gyach-quote.el ends here
