;;; gyach-url.el --- URL functions for ElGyach

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

(require 'gyach-button)

(defvar gyach-url-list '())

(defun gyach-url-list ()
  gyach-url-list)

(defun gyach-last-url ()
  (car (gyach-url-list)))

(defun gyach-preoutput-url-sniff (string)
  (when (not (null (string-match gyach-button-url-regexp string)))
    (add-to-list 'gyach-url-list 
		 (replace-regexp-in-string "\\\\n" "" (match-string 0 string))))      
  string)

(defun gyach-custom-OPENURL (proc argument)
  "Open the last URL using `browse-url-browser-function'.
If a numeric argument is give, eg. /openurl n, then the n-th last
URL is opened instead."
  (when (string-match "\\([0-9]+\\)?" argument)
    (let ((url-n)
	  (url)
	  (arg (match-string 1 argument)))
      (if (null arg)
	  (setq url-n 0)
	  (setq url-n (- (string-to-number arg) 1)))
      (setq url (car (nthcdr url-n (gyach-url-list))))
      (if (null url)
	  (message "No such URL available.")
	  (browse-url url)))))

(provide 'gyach-url)

;;; gyach-url.el ends here
