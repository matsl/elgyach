;;; gyach-ignore.el --- ElGyach facilities for ignoring posters

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

(defvar gyach-ignorables '())

(defun gyach-ignorables ()
  gyach-ignorables)

(defun gyach-custom-FOOLS (proc argument)
  "Announce to the room how many fools you were able to ignore."
  (let ((fool-count (length (gyach-ignorables))))
    (comint-send-string 
     proc  
     (case fool-count
       (0 ": hmmm... sadly, no fools have been ignored\n\n")
       (1 (format ": %s is the only fool in my ignore list!\n\n" (car (gyach-ignorables))))
       (otherwise 
	(format ": %d fools currently ignored (last fool was %s)\n\n" 
		fool-count (car (gyach-ignorables))))))))

(defun gyach-custom-IGNORE (proc argument)
  (gyach-ignore argument))

(defun gyach-custom-UNIGNORE (proc argument)
  (gyach-unignore argument))

(defun gyach-ignore (user)
  (string-match (concat "\\(" gyach-username-regexp "\\)") user)
  (let ((clean-user (match-string 1 user)))
    (when clean-user (add-to-list 'gyach-ignorables clean-user))))

(defun gyach-unignore (user)
  (string-match (concat "\\(" gyach-username-regexp "\\)") user)
  (let ((clean-user (match-string 1 user)))
    (when clean-user (setq gyach-ignorables (remove clean-user gyach-ignorables)))))

(defun gyach-is-ignorable (user)
  (member-ignore-case user (gyach-ignorables)))

(defun gyach-ignore-import-inferior (file)
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

(provide 'gyach-ignore)

;;; gyach-ignore.el ends here
