;;; gyach-button.el --- clickable buttons etc.

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mbkennedy@austin.rr.com>
;; Keywords: processes

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

(require 'gyach-room)

;; This is right out of ERC (we all love ERC):

(defcustom gyach-button-url-regexp "\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]"
  "Regular expression that matches URLs."
  :group 'gyach
  :type 'regexp)

(defconst gyach-username-profile-url "http://profiles.yahoo.com/"
  "URL to use for user profiles.")

(defun gyach-username-button (username)
  (when username
    (browse-url (concat gyach-username-profile-url username))))

(defun gyach-button-username-replace (start end)
  (save-excursion
    (narrow-to-region start end)
    (dolist (user (gyach-room-list))
      (goto-char (point-min))
      (while (search-forward-regexp (concat "\\(" user "\\)") (point-max) nil)
	(make-text-button (match-beginning 1) (match-end 1)
			  :type 'help-xref
			  'help-echo "mouse-2, RET: user profile"
			  'help-function 'gyach-username-button
			  'help-args (list (buffer-substring (match-beginning 1)
							     (match-end 1))))))))

(provide 'gyach-button)

;;; gyach-button.el ends here
