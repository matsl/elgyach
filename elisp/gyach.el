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
(require 'comint)

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


(defvar gyach-program-arguments '()
  "List of other arguments to pass to the gyach sub-process.")

(defvar gyach-mode-font-lock-keywords
  (list 
   '("^\\(\\w+\\)" . 1))
  "Font lock keywords for gyach-mode.")

(defvar gyach-username-regexp "[a-zA-Z0-9_.@\\-]+")

(defconst gyach-version "ElGyach 0.1.1, the GNU Emacs Lisp interface to Yahoo! Chat http://savannah.nongnu.org/projects/elgyach/")

(defvar gyach-ignorables '()
  "List of fools to ignore")

(defvar gyach-highlightables '()
  "List of fools to highlight")

(defvar gyach-mode-hook '()) 

(defvar gyach-output-hook '()
  "List of functions taking two arguments (the beginning and end
  a region) which are called on text outputed to the buffer" )

(defvar gyach-room-list '())

(defvar gyach-last-url nil)

(defvar gyach-url-list '())

;;; Code:

(defun gyach-room-list ()
  gyach-room-list)

(defun gyach-save () 
  "Save El-Gyach setting to file. Currently this includes
`gyach-ignorables'.  The file to use is given by the variable
`gyach-save-file'."
  (with-temp-buffer
    (erase-buffer)
    (print gyach-ignorables (current-buffer))
    (write-file (expand-file-name gyach-save-file))))

(defun gyach-load ()
  "Load El-Gyach settings from file. Currently this includes
`gyach-ignorables'. The file to use is given by the variable
`gyach-save-file'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name gyach-save-file))
    (goto-char (point-min))
    (setq gyach-ignorables (read (current-buffer)))))

(defun gyach-list ()
  "List current chatters."
  (save-excursion
    (pop-to-buffer "*gyach-messages*")
    (erase-buffer)
    (dolist (user (sort (gyach-room-list) 'string-lessp))
      (insert (format "%s\n" user)))))

(define-derived-mode gyach-mode comint-mode "gyach"
  (set (make-local-variable 'font-lock-keywords)
       '(gyach-mode-font-lock-keywords)))

(defun gyach-prompt-for-details ()
  "Prompt for the details of the connection. Save the data in the
global variables gyach-yahoo-username, gyach-yahoo-password,
gyach-yahoo-room and gyach-yahoo-server."
  (let (u p r s)
    (setq u (read-string "Username: " (or gyach-yahoo-username "")))
    (setq p (read-passwd "Password: " nil (or gyach-yahoo-password "")))
    (setq r (read-string "Room: " (or gyach-yahoo-room "Linux, FreeBSD, Solaris:1")))
    (setq s (read-string "Server: " (or gyach-yahoo-server "scs.yahoo.com")))
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

(defun gyach-unescape-newlines (string)
  "Replace instances of \\n with \\\\n in STRING."
  (let ((result))
    (setq result (replace-regexp-in-string (concat "\\\\" "n") "\n" string))
    result))

(defun gyach-simple-send (proc string)
  "Gyach function for sending to PROC input STRING.
This just sends STRING plus a newline. To override this, set the
hook `comint-input-sender'. This is basically a gyach custom
version of `comint-simple-send'"
  (if gyach-dont-send
      (progn
	(cond ((equal gyach-command 'version)
	       (setq gyach-dont-send nil)
	       (gyach-simple-send proc (concat ": is conversing with you via " gyach-version)))
	      ;; ignore
	      ((equal gyach-command 'ignore)
	       (add-to-list 'gyach-ignorables gyach-command-arg)
	       (message "Ignoring %s" gyach-command-arg))
	      ;; unignore
	      ((equal gyach-command 'unignore)
	       (setq gyach-ignorables (remove gyach-command-arg gyach-ignorables))
	       (message "Unignoring %s" gyach-command-arg))
	      ;; save
	      ((equal gyach-command 'save)
	       (gyach-save))
	      ;; load
	      ((equal gyach-command 'load)
	       (gyach-load))
	      ;; list 
	      ((equal gyach-command 'list)
	       (gyach-list))
	      ;; highlight
	      ((equal gyach-command 'highlight)
	       (add-to-list 'gyach-highlightables gyach-command-arg)
	       (message "Highlighting %s" gyach-command-arg))
	      ;; unhighlight
	      ((equal gyach-command 'unhighlight)
	       (setq gyach-highlightables (remove gyach-command-arg gyach-highlightables))
	       (message "Unhighlighting %s" gyach-command-arg))
	      ((equal gyach-command 'custom)
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
  (gyach-prompt-for-details)

  (if (not (comint-check-proc buffer))
      (let ((program (or gyach-program-name "gyach-text"))
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
  (set (make-local-variable 'comint-process-echos) nil)
  (set (make-local-variable 'comint-input-sender) 'gyach-simple-send)
  (add-hook 'comint-preoutput-filter-functions 'gyach-preoutput-filter-functions nil t)
  (add-hook 'comint-input-filter-functions 'gyach-input-filter-functions nil t)
  (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom t)
  (add-hook 'comint-output-filter-functions 'gyach-output-filter-functions nil t))


(defun gyach-preoutput-filter-functions (string)
  "Clean up text before insertion"
  ;; look for URLs
  (when (not (null (string-match gyach-button-url-regexp string)))
    (add-to-list 'gyach-url-list (replace-regexp-in-string "\\\\n" "" (match-string 0 string))))
  ;; process kinds of output from sub-process
  (cond ((string-match (concat "^\\(" gyach-username-regexp "\\): \\(.*\\)") string)
	 (let ((user (downcase (match-string 1 string)))
	       (text (match-string 2 string)))
	   (if (member-ignore-case user gyach-ignorables)
	       ""
	     (format "<%s> %s" 
		     (if (member-ignore-case user gyach-highlightables)
			 (propertize user 'face 'gyach-username-face)
		       (propertize user 'face 'gyach-username-face))
		     (progn
		       (setq text (gyach-unescape-newlines text))
		       (gyach-replace-usernames 
			(if (equal user gyach-yahoo-username)
			    (propertize text 'face 'gyach-input-face)
			    (propertize text 'face 'gyach-default-face))
			'gyach-username-face))))))
	((string-match (concat "^\\*\\ \\(" gyach-username-regexp "\\)\\(.*\\)") string)
	 (let ((user (downcase (match-string 1 string)))
	       (text (match-string 2 string)))
	   (if (member-ignore-case user gyach-ignorables)
	       ""
	     (format "* %s%s" 
		     (if (member-ignore-case user gyach-highlightables)
			 (propertize user 'face 'gyach-username-face)
			 (propertize user 'face 'gyach-username-face))
		     (gyach-replace-usernames 
		      (propertize (gyach-unescape-newlines text) 'face 'gyach-action-face)
		      'gyach-username-face)))))
	((string-match (concat "^\\(" gyach-username-regexp "\\) \\(enters\\|leaves\\) the room") string)
	 (let ((user (downcase (match-string 1 string)))
	       (event (match-string 2 string)))
	   (cond ((equal event "enters")
		  (message "Adding %s to room list" user)
		  (add-to-list 'gyach-room-list user)
		  (message "%d people in room" (length gyach-room-list)))
		 ((equal event "leaves")
		  (message "Removing %s from room list" user);
		  (setq gyach-room-list (remove user gyach-room-list))
		  (message "%d people in room" (length gyach-room-list)))))
	 (if (not gyach-suppress-enter-leave-messages)
	     (propertize 
	      (concat "*** " (replace-regexp-in-string "\\\\n" "" string)) 
	      'face 'gyach-event-face)
	     ""))
	(t 
	 string)))

(defun gyach-output-filter-functions (string)
  (dolist (h gyach-output-hook)
    (funcall h comint-last-output-start (point))))

(defun gyach-input-filter-functions (string)
  "Intercept gyach.el command overrides."
  (cond ((string-match "^/version" string)
	 (setq gyach-dont-send t
	       gyach-command 'version
	       gyach-command-arg nil))
	((string-match (concat "^/ignore\\ +\\(" gyach-username-regexp "\\)") string)
	 (setq gyach-dont-send t
	       gyach-command 'ignore
	       gyach-command-arg (downcase (match-string 1 string))))
	((string-match (concat "^/unignore\\ +\\(" gyach-username-regexp "\\)") string)
	 (setq gyach-dont-send t
	       gyach-command 'unignore
	       gyach-command-arg (downcase (match-string 1 string))))
	((string-match "^/save" string)
	 (setq gyach-dont-send t
	       gyach-command 'save
	       gyach-command-arg nil))
	((string-match "^/load" string)
	 (setq gyach-dont-send t
	       gyach-command 'load
	       gyach-command-arg nil))
	((string-match "^/list" string)
	 (setq gyach-dont-send t
	       gyach-command 'list
	       gyach-command-arg nil))
	((string-match (concat "^/highlight\\ +\\(" gyach-username-regexp "\\)") string)
	 (setq gyach-dont-send t
	       gyach-command 'highlight
	       gyach-command-arg (downcase (match-string 1 string))))
	((string-match (concat "^/unhighlight\\ +\\(" gyach-username-regexp "\\)") string)
	 (setq gyach-dont-send t
	       gyach-command 'unhighlight
	       gyach-command-arg (downcase (match-string 1 string))))
	;; pass-through commands 
	((string-match "^/\\(tell\\|think\\|quit\\|\\join\\)\\ +" string)
	 (setq gyach-dont-send nil))
	;; user-custom commands
	((string-match "^/\\([a-zA-Z]+\\)\\ ?\\(.*\\)" string)
	 (setq gyach-dont-send t
	       gyach-command 'custom
	       gyach-command-arg (cons (downcase (match-string 1 string)) (match-string 2 string))))
	(t
	  (setq gyach-dont-send nil))))


(defun gyach-clean-username (username)
  username)

(defun gyach-replace-usernames (string username-face)
  (with-temp-buffer
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (dolist (user (gyach-room-list))
      (while (search-forward user  nil t)
	(replace-match (propertize (gyach-clean-username user) 'face username-face) nil t)))
    (buffer-string)))

(defun gyach-url-list ()
  gyach-url-list)

(defun gyach-last-url ()
  (car (gyach-url-list)))

(provide 'gyach)

;;; gyach.el ends here
