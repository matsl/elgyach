;;; gyach-extra.el --- extra commands for gyach.el

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@killr.ath.cx>
;; Keywords: convenience

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

(require 'comint)

(defun gyach-custom-YOW (proc argument)
  "Chatters need Zippy wisdom."
  (let ((quote (yow)))
    (setq quote (replace-regexp-in-string "\n" " " quote))
    (comint-send-string proc (format "%s\n\n" quote))))

(defun gyach-custom-EVAL (proc argument)
  "Evaluate Emacs for all to stand in awe"
  (let ((result))
    (with-temp-buffer
      (erase-buffer)
      (eval-expression (read argument) t)
      (goto-char (point-min))
      (insert (format "%s => " argument))
      (set-mark (point-min))
      (goto-char (point-max))
      (setq result (format "%s\n\n" (buffer-substring (mark) (point)))))
    (comint-send-string proc result)))

(defun gyach-output-fill (start end)
  (fill-region start end))
  
;; if you have Oort(?) Gnus installed, try the following:

;; (add-to-list 'gyach-output-hook
;; 	     'smiley-region)

(provide 'gyach-extra)

;;; gyach-extra.el ends here
