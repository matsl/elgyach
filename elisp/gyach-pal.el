;;; gyach-pal.el --- ElGyach facilities for keeping track of pals

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


(defcustom gyach-pal-list nil
  "List of people to treat as pals.
This list is appended to the `gyach-room-list' for completion.
The reason being, that its not uncommon to get a PM from a friend
who isn't in the room you're in at the moment.  Thus
`gyach-pal-list' serves as a whitelisting feature"
  :type 'list
  :group 'gyach)

(defun gyach-pal-list ()
  gyach-pal-list)

(defun gyach-custom-PAL (proc argument)
  (gyach-pal argument))

(defun gyach-custom-UNPAL (proc argument)
  (gyach-unpal argument))

(defun gyach-pal (user)
  (string-match (concat "\\(" gyach-username-regexp "\\)") user)
  (let ((clean-user (match-string 1 user)))
    (when clean-user (add-to-list 'gyach-pal-list clean-user))))

(defun gyach-unpal (user)
  (string-match (concat "\\(" gyach-username-regexp "\\)") user)
  (let ((clean-user (match-string 1 user)))
    (when clean-user (setq gyach-pal-list (remove clean-user gyach-pal-list)))))

(defun gyach-is-pal (user)
  (member-ignore-case user (gyach-pal-list)))

(provide 'gyach-ignore)

;;; gyach-ignore.el ends here
