;;; gyach-state.el --- ElGyach state database

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

(defvar gyach-ignorables '()
  "List of fools to ignore")

(defun gyach-save () 
  "Save El-Gyach setting to file. Currently this includes
`gyach-ignorables'.  The file to use is given by the variable
`gyach-save-file'."
  (with-temp-buffer
    (erase-buffer)
    (print gyach-ignorables (current-buffer))
    (write-file (expand-file-name gyach-save-file))))

(defun gyach-load ()
  "Load El-Gyach settings from file. Currently this includes
`gyach-ignorables'. The file to use is given by the variable
`gyach-save-file'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name gyach-save-file))
    (goto-char (point-min))
    (setq gyach-ignorables (read (current-buffer)))))

(provide 'gyach-state)

;;; gyach-state.el ends here
