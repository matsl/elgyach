;;; gyach-room.el --- ElGyach room list

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mbkennedy@austin.rr.com>
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

(require 'gyach-events)

(defconst gyach-message-buffer-name "*gyach-messages*"
  "The buffer to send room list information to.")

(defvar gyach-room-list nil)

(set (make-local-variable 'gyach-room-list) '())

(defun gyach-room-list ()
  gyach-room-list)

(defun gyach-room-list-add (user)
  (add-to-list 'gyach-room-list user))

(defun gyach-room-list-remove (user)
  (setq gyach-room-list (remove user gyach-room-list)))

;; This is pretty rudimentary so far.  Ideally each username would be
;; clickable (perhaps displaying their profile

(defun gyach-custom-NAMES (proc argument)
  "List current chatters."
  (with-output-to-temp-buffer gyach-message-buffer-name
    (set-buffer gyach-message-buffer-name)
    (dolist (user (gyach-room-list))
      (insert (format "%s\n" user)))))

(defun gyach-enter-event-handler (event-type argument)
  (when (equal event-type 'enter)
    (gyach-room-list-add argument))
  t)

(defun gyach-leave-event-handler (event-type argument)
  (when (equal event-type 'leave)
    (gyach-room-list-remove argument))
  t)

(add-to-list 'gyach-event-hook 'gyach-enter-event-handler)
(add-to-list 'gyach-event-hook 'gyach-leave-event-handler)

(provide 'gyach-room)

;;; gyach-room.el ends here
