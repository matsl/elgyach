;;; gyach-custom.el --- ElGyach customizable variables

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

(defgroup gyach nil
  "ElGyach Yahoo! Chat interface."
  :link '(url-link "http://savannah.nongnu.org/projects/elgyach/")
  :group 'processes)

(defcustom gyach-yahoo-username nil
  "Yahoo! Chat username."
  :group 'gyach
  :type 'string)

(defcustom gyach-yahoo-password nil
  "Yahoo! Chat password."
  :group 'gyach
  :type 'string)

(defcustom gyach-yahoo-room "Linux, FreeBSD, Solaris:1"
  "Yahoo! Chat room.
`gyach-yahoo-room' defines the initial or default room to enter"
  :group 'gyach
  :type 'string)

(defcustom gyach-yahoo-server "scs.yahoo.com"
  "Yahoo! Chat server."
  :group 'gyach
  :type 'string)

(defcustom gyach-program-name "elgyach"
  "Gyach program to use."
  :group 'gyach
  :type 'string)

(defcustom gyach-save-file "~/.elgyach.el"
  "There gyach.el should save and load state."
  :group 'gyach
  :type 'string)

(defcustom gyach-suppress-enter-leave-messages nil
  "If non-nil, suppress the display of enter/leave room messages"
  :group 'gyach
  :type 'boolean)

(defcustom gyach-program-arguments '()
  "List of other arguments to pass to the gyach sub-process."
  :group 'gyach
  :type 'string)

(defcustom gyach-logo-file "/usr/share/elgyach/elgyach-logo.png"
  "Path to ElGyach logo."
  :group 'gyach
  :type 'string)

(defgroup gyach-hooks nil
  "Hooks for customization in ElGyach")

(defcustom gyach-mode-hook '()
  "List of functions to call after `gyach-mode' is called."
  :group 'gyach-hooks
  :type 'hook)

(defcustom gyach-output-hook '()
  "List of functions taking two arguments numeric arguments.
`gyach-output-hook' is a set of functions to call on text after
it is sent to the buffer.  Each function takes two arguments: the
beginning and end the region of text"
  :group 'gyach-hooks
  :type 'hook)

(defcustom gyach-preoutput-hook '()
  "List of functions taking a single string argument.
`gyach-preoutput-hook' is a set of functions which are called on
the raw text as it comes in from the sub-process.  Users of this
hook should take care to preserve the initial part of the
text (everything before the text including the username and
action character) and also the form of enter/leave messages."
  :group 'gyach-hooks
  :type 'hook)

(provide 'gyach-custom)

;;; gyach-custom.el ends here
