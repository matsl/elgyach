;;; mpg123el-show.el --- functions for interacting with mpg123.el

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mbkennedy@austin.rr.com>
;; Keywords: multimedia

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

(require 'mpg123)

(defcustom mpg123el-show-prefix 
  (concat "GNU Emacs " emacs-version " is playing: ")
  "The prefix to use when showing the current track name."
  :group 'mpg123el-show
  :type 'string)
  
(defun mpg123el-show (&optional arg display)
  "Return or display current playing mpg123.el track name. With an argument, insert it."
  (interactive "P\np")
  (let ((name)
	(result))
    (save-excursion
      (set-buffer (get-buffer mpg123*buffer))
      (goto-char mpg123*cur-play-marker)
      (looking-at "\\ *\\([0-9]+\\)")
      (setq name (mpg123:get-music-info 
		  (string-to-number (buffer-substring 
				     (match-beginning 1) 
				     (match-end 1)))
		  'name)))
    (setq result (concat mpg123el-show-prefix name))
    (cond (arg 
	   (insert result))
	  ((not display)
	   result)
	  (t 
	   (message result)))))

(provide 'mpg123el-show)

;;; mpg123el-show.el ends here
