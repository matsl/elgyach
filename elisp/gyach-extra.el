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

(defun gyach-custom-FOOLS (proc argument)
  "Announce to the room how many fools you were able to ignore."
  (comint-send-string proc (format ": %d fools currently ignored (last fool was %s)\n\n" 
				   (length gyach-ignorables) (car gyach-ignorables))))

(defun gyach-custom-YOW (proc argument)
  "Chatters need Zippy wisdom."
  (let ((quote (yow)))
    (setq quote (replace-regexp-in-string "\n" " " quote))
    (comint-send-string proc (format "%s\n\n" quote))))

(defun gyach-import-inferior (file)
  "Import an ignore list (from gyach/curphoo/curfloo -- anything with an ignore per line)."
  (interactive
   (list (read-file-name (format "Ignore list to merge with %s: " gyach-save-file) nil nil t)))
  (when (null file)
    (error "You cannot specify nil for the filename!"))
  (save-excursion
    (with-temp-buffer
      (insert-file file)
      (goto-char (point-min))
      (let ((count 0))
	(while (not (equal (forward-line) 1))
	  (let ((beg)
		(end))
	    (beginning-of-line)
	    (setq beg (point))
	    (end-of-line)
	    (setq end (point))
	    (let ((line (buffer-substring beg end)))
	      (when (> (length line) 0)
		(progn 
		  (setq gyach-ignorables (cons line gyach-ignorables))
		  (setq count (1+ count)))))))
	(message "%d fools appended to your ignore list (%d total)" count (length gyach-ignorables))))))

;; (add-hook 'gyach-hook 
;; 	  '(lambda ()
;; 	    (setq 
;; 	     fill-column 90
;; 	     fill-prefix "    ")))

(provide 'gyach-extra)

;;; gyach-extra.el ends here

