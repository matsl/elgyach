;;; gyach-away.el --- ElGyach support for away messages

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

(require 'time-date)

(defvar gyach-last-seen nil
  "When the user last issued an away command.")

(defvar gyach-away-messages
  '((away . "is away")
    (back . "is back"))
  "Associative list of away and back messages")

(defcustom gyach-away-default-excuse 
  '(lambda () 
     "be back later")
  "The default reason to give if no argument is given to the
  /away command"
  :group 'gyach
  :type 'function)

(defun gyach-custom-AWAY (proc argument)
  "ElGyach away message."
  (setq gyach-last-seen (current-time))
  (let* ((arg-given (string-match "\\W*\\(\\w+.*\\)\\W*" argument))
	 (excuse (and arg-given 
		       (match-string 1 argument))))
    (comint-send-string proc (format ": %s ( %s ).\n\n"
				     (or (cdr (assoc 'away gyach-away-messages))
					 "is away")
				     (or excuse
					 (apply gyach-away-default-excuse nil)
					 "bbl")))))

(defun gyach-custom-BACK (proc argument)
  "ElGyach back message"
  (when gyach-last-seen
    (let ((time-gone (time-to-seconds (time-since gyach-last-seen))))
      (comint-send-string
       proc 
       (prog1 
	   (format ": %s ( gone %d seconds ).\n\n" 
		   (or (cdr (assoc 'back gyach-away-messages))
		       "is back")
		   time-gone)
	 (setq gyach-last-seen nil))))))

(defun gyach-away-bofh-fortune ()
  "You can set `gyach-default-away-excuse' to this and get BOFH
excuses (for leaving the room, you know?)"
  (let ((output (shell-command-to-string "fortune bofh-excuses")))
    (replace-regexp-in-string "\n\n" " "  (substring output 0 (- (length output) 1)))))

(provide 'gyach-away)

;;; gyach-away.el ends here
