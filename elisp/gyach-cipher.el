;;; gyach-cipher.el --- Cipher interface for ElGyach

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@killr.ath.cx>
;; Keywords: tools

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

;; How users generate and exchange keys is not handled by this code.
;; Here's some hints though:
;;
;; On Unix: 
;;
;;    dd if=/dev/random bs=1 count=16 2>/dev/null |od -tx1 |head -1 |\
;;      cut -f2- -d\  |sed 's, ,,g'
;;
;; (Only works for 16 byte keys)
;;
;; Email each other your cipher keys using GnuPG.

(require 'cl)

(defvar gyach-cipher-map
  '(("emacs_divine" 128 "b3e6bac81fed6775fab2f1809ef35fc1")
    ("carameldelite18" 128 "b3e6bac81fed6775fab2f1809ef35fc1")
    ("li1_bastard" 128 "b3e6bac81fed6775fab2f1809ef35fc1")
    ("duralinux" 128 "b3e6bac81fed6775fab2f1809ef35fc1")))

(defun gyach-cipher-frame-message (message blockbits)
  "Frame MESSAGE so that the message length is a muliple of BLOCKBITS."
  (let ((remainder-bits (mod (* 8 (length message)) blockbits)))
    (concat message (make-string (/ (- blockbits remainder-bits) 8)  ?\x20))))

(defun gyach-encipher (message blockbits key)
  (let ((msg (gyach-cipher-frame-message message blockbits)))
    (rijndael-block-encrypt msg key 'ecb blockbits)
    msg))

(defun gyach-decipher (message blockbits key)
  (let ((msg message))
    (rijndael-block-decrypt msg key 'ecb blockbits)
    msg))

(defun gyach-do-encipher (username message)
  (let* ((config (assoc username gyach-cipher-map))
	 (key (rijndael-hexstring-to-bitstring (caddr config)))
	 (block-length (cadr config)))
    (format "[%s] %s" "rijndael" 
	    (rijndael-bitstring-to-hexstring
	     (gyach-encipher message block-length 
			     (rijndael-make-key block-length key))))))

(defun gyach-custom-CIPHER (proc argument)
  "Send an encrypted message to a user.
Format of command is: /cipher username message"
  (string-match (concat "\\(" gyach-username-regexp "\\) \\(.*\\)") argument)
  (let ((username (match-string 1 argument))
	(message (match-string 2 argument)))
    (comint-send-string proc 
			(format "/tell %s %s\n\n" username
				(gyach-do-encipher username message)))))

(defun gyach-custom-PCIPHER (proc argument)
  "Send an encrypted message to all.
Format of command is: /pcipher message"
  (comint-send-string proc 
		      (format "%s\n\n" (gyach-do-encipher gyach-yahoo-username argument))))

(provide 'gyach-cipher)

;;; gyach-cipher.el ends here
