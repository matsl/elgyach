;;; gyach-highlight.el --- ElGyach post highlighting facility

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

(defvar gyach-highlightables '())

(defun gyach-highlightables ()
  gyach-highlightables)

(defun gyach-custom-HIGHLIGHT (proc argument)
  (gyach-highlight argument))

(defun gyach-custom-UNHIGHLIGHT (proc argument)
  (gyach-unhighlight argument))

(defun gyach-highlight (user)
  (string-match (concat "\\(" gyach-username-regexp "\\)") user)
  (add-to-list 'gyach-highlightables (match-string 1 user)))

(defun gyach-unhighlight (user)
  (string-match (concat "\\(" gyach-username-regexp "\\)") user)
  (setq gyach-highlightables (remove (match-string 1 user) gyach-highlightables)))

(defun gyach-is-highlightable (username post)
  (cond ((member-ignore-case username (gyach-highlightables))
	 t)
	(t 
	 nil)))
	
(provide 'gyach-highlight)

;;; gyach-highlight.el ends here
