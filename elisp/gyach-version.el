;;; gyach-version.el --- version from build system

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

(require 'comint)

(defvar gyach-version "0.2"
  "ElGyach version from build system.")

(defconst gyach-publicity-string
  (concat "ElGyach " gyach-version 
	  ", the GNU Emacs Lisp interface to Yahoo! Chat "
	  "http://savannah.nongnu.org/projects/elgyach/")
  "Client advertisement string.  There is no real reason to
change this.")

(defun gyach-custom-VERSION (proc argument)
  "Advertise to other users what client you're chatting with."
  (comint-send-string proc (format ": is conversing with you via %s\n\n" 
				   gyach-publicity-string)))

(provide 'gyach-version)

;;; gyach-version.el ends here
