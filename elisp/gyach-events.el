;;; gyach-events.el --- ElHyach event handling

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

(defcustom gyach-event-hook '()
  "List of functions taking two arguments.
`gyach-event-hook' is a set of functions taking two arguments:
the event type and and event argument."
  :group 'gyach-hooks
  :type 'hook)

(defun gyach-process-events (event-type argument)
  "Process each event hooks in `gyach-event-hook' until one returns nil"
  (catch 'abort
    (dolist (h gyach-event-hook)
      (when (null (funcall h event-type argument))
	(throw 'abort nil)))))



(provide 'gyach-events)

;;; gyach-events.el ends here
