;;; gyach.el --- emacs interface to gyach

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
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

(require 'gyach-faces)
(require 'gyach-button)
(require 'gyach-state)
(require 'gyach-extra)
(require 'gyach-url)
(require 'gyach-version)
(require 'gyach-room)
(require 'gyach-ignore)
(require 'gyach-events)
(require 'gyach-highlight)
(require 'gyach-complete)
(require 'gyach-custom)

(require 'comint)

(defconst gyach-username-regexp "[a-zA-Z0-9_.@\\-]+")

(defvar gyach-dont-send nil)
(defvar gyach-command nil)
(defvar gyach-command-arg nil)

(defvar gyach-buffer nil)

;;; Code:

(define-derived-mode gyach-mode comint-mode "gyach")

(defun gyach-prompt-for-details ()
  "Prompt for the details of the connection. Save the data in the
global variables gyach-yahoo-username, gyach-yahoo-password,
gyach-yahoo-room and gyach-yahoo-server."
  (let (u p r s)
    (setq u (or gyach-yahoo-username (read-string "Username: " "")))
    (setq p (or gyach-yahoo-password (read-passwd "Password: " nil "")))
    (setq r (or gyach-yahoo-room (read-string "Room: " "Linux, FreeBSD, Solaris:1")))
    (setq s (or gyach-yahoo-server (read-string "Server: " "scs.yahoo.com")))
    (setq
     gyach-yahoo-username u
     gyach-yahoo-password p
     gyach-yahoo-room r
     gyach-yahoo-server s)))

(defun gyach-make-program-arguments ()
  "Make a list of arguments to pass to the gyach sub-process"
  (append (list "-u" gyach-yahoo-username)
	  (list "-p" gyach-yahoo-password)
	  (list "r" gyach-yahoo-room)
	  (list "s" gyach-yahoo-server)
	  gyach-program-arguments))

(defun gyach-simple-send (proc string)
  "Gyach function for sending to PROC input STRING.
This just sends STRING plus a newline. To override this, set the
hook `comint-input-sender'. This is basically a gyach custom
version of `comint-simple-send'"
  (if gyach-dont-send
      (progn
	(cond ((equal gyach-command 'custom)
	       (gyach-eval-custom proc (car gyach-command-arg) (cdr gyach-command-arg)))))
      (progn
	(comint-send-string proc string)
	(if comint-input-sender-no-newline
	    (if (not (string-equal string ""))
		(process-send-eof))
	    (comint-send-string proc "\n")))))

(defun gyach-eval-custom (proc command argument)
  "Try to evaluate command with argument."
  (let ((command-function (intern (concat "gyach-custom-" (upcase command)))))
    (when (fboundp command-function)
      (funcall command-function proc argument))))

;;;###autoload 
(defun elgyach (&optional buffer)
  (interactive
   (list (and current-prefix-arg 
	      (read-buffer "Gyach buffer: " "*gyach*"))))
  (when (null buffer)
    (setq buffer "*gyach*"))

  (let ((the-buffer (get-buffer-create buffer)))
    (set-buffer the-buffer)
    (when (file-exists-p gyach-logo-file)
      (insert-image (create-image gyach-logo-file)))
    (insert "\n"))

  (gyach-prompt-for-details)

  (if (not (comint-check-proc buffer))
      (let ((program (or gyach-program-name "elgyach"))
	    shell-buffer)
	(save-excursion 
	  (set-buffer (apply 'make-comint-in-buffer 
			     "gyach" 
			     buffer 
			     gyach-program-name 
			     nil 
 			     (gyach-make-program-arguments)))
	  (setq gyach-buffer (current-buffer))
	  (gyach-mode)
	  (run-hooks 'gyach-mode-hook))
	(pop-to-buffer gyach-buffer))
      (pop-to-buffer buffer))
  ;; configure our comint usage now that we have a buffer
  (set (make-local-variable 'comint-process-echoes) nil)
  (set (make-local-variable 'comint-input-sender) 'gyach-simple-send)
  (set (make-local-variable 'scroll-conservatively) 0)
  (set (make-local-variable 'comint-scroll-show-maximum-output) nil)
  (add-hook 'comint-preoutput-filter-functions 'gyach-preoutput-filter-functions nil t)
  (add-hook 'comint-input-filter-functions 'gyach-input-filter-functions nil t)
  (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom t)
  (add-hook 'comint-output-filter-functions 'gyach-output-filter-functions nil t))


(defun gyach-preoutput-filter-functions (string)
  "Clean up text before insertion"
  (dolist (h gyach-preoutput-hook)
    (setq string (funcall h string)))
  (cond ;; plain messages
        ((string-match (concat "^\\(" gyach-username-regexp "\\): \\(.*\\)") string)
	 (let ((user (downcase (match-string 1 string)))
	       (text (match-string 2 string)))
	   ;; be very aggressive (don't rely on the room list package from yahoo)
	   (gyach-room-list-add user)
	   (if (gyach-is-ignorable user) "" (gyach-make-rendered-text 'default user text))))
	;; action messages
	((string-match (concat "^\\*\\ \\(" gyach-username-regexp "\\)\\(.*\\)") string)
	 (let ((user (downcase (match-string 1 string)))
	       (text (match-string 2 string)))
	   ;; be very aggressive (don't rely on the room list package from yahoo)
	   (gyach-room-list-add user)
	   (if (gyach-is-ignorable user) "" (gyach-make-rendered-text 'action user text))))
	;; leave/enter events
	((string-match (concat "^\\(" gyach-username-regexp "\\) \\(enters\\|leaves\\)") string)
	 (let ((user (downcase (match-string 1 string)))
	       (event (match-string 2 string))
	       (event-type))
	   (cond ((equal event "enters")
		  (setq event-type 'enter))
		 ((equal event "leaves")
		  (setq event-type 'leave)))
	   (gyach-process-events event-type user)
	   (if (not gyach-suppress-enter-leave-messages)
	       (gyach-make-rendered-text event-type user nil))))
	;; initialization messages
	((string-match (concat "^init room \\(.*\\)") string)
	 (let ((room-list (split-string (replace-regexp-in-string 
					 "\\\\n" "" 
					 (downcase (match-string 1 string))) "," t)))
	   (dolist (user room-list)
	     (gyach-room-list-add user))
	   (gyach-make-rendered-text 'names nil room-list)))
	;; server messages
	((string-match "^\\*\\*\\* \\(.*\\)" string)
	 (gyach-make-rendered-text 'server nil (match-string 1 string)))
	;; private messages
	((string-match (concat "^\\(" gyach-username-regexp "\\) <private to " 
			       gyach-yahoo-username "> \\(.*\\)") 
		       string)
	 (let ((user (downcase (match-string 1 string)))
	       (text (match-string 2 string)))
	   (if (gyach-is-ignorable user) "" (gyach-make-rendered-text 'private user text))))
	(t 
	 string)))

(defun gyach-output-filter-functions (string)
  (dolist (h gyach-output-hook)
    ;; FIXME: shit what was i thinking?
    (funcall h comint-last-output-start (point))))

(defun gyach-input-filter-functions (string)
  "Intercept gyach.el command overrides."
  (cond ;; pass-through commands 
	((string-match "^/\\(tell\\|think\\|quit\\|\\join\\)\\ +" string)
	 (setq gyach-dont-send nil))
	;; user-custom commands
	((string-match "^/\\([a-zA-Z]+\\)\\ ?\\(.*\\)" string)
	 (setq gyach-dont-send t
	       gyach-command 'custom
	       gyach-command-arg (cons (downcase (match-string 1 string)) (match-string 2 string))))
	(t
	  (setq gyach-dont-send nil))))

(define-key comint-mode-map [?\M-\t] 'ispell-complete-word)
(define-key comint-mode-map [?\t] 'gyach-complete)

(add-to-list 'gyach-preoutput-hook 
	     'gyach-preoutput-url-sniff)

(add-to-list 'gyach-output-hook
	     'gyach-output-fill)

(provide 'gyach)

;;; gyach.el ends here
