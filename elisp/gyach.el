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

(require 'gyach-custom)
(require 'gyach-faces)
(require 'comint)
(require 'match)
(require 'utility)
(require 'cl)

(defvar gyach-dont-send nil)
(defvar gyach-command nil)
(defvar gyach-command-arg nil)

(defvar gyach-buffer nil)

;;; Code:

(define-derived-mode gyach-mode comint-mode "gyach")

(defvar comint-input-sender-no-newline nil)

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

(defun* gyach-process (&optional (buffer (current-buffer)))
  (get-buffer-process buffer))

;;;###autoload 
(defun elgyach (&optional buffer)
  (interactive
   (list (and current-prefix-arg 
	      (read-buffer "Gyach buffer: " "*elgyach*"))))
  (when (null buffer)
    (setq buffer "*gyach*"))
  (let ((the-buffer (get-buffer-create buffer)))
    (set-buffer the-buffer)
    (when (and gyach-logo-file (file-exists-p gyach-logo-file))
      (insert-image (create-image gyach-logo-file)))
    (insert "\n"))

; (gyach-custom-restore-state-maybe)
; (gyach-prompt-for-details)
  (if (not (comint-check-proc buffer))
      (let ((program (or gyach-program-name "elgyach"))
	    shell-buffer)
	(save-excursion 
	  (set-buffer (make-comint-in-buffer "elgyach" buffer gyach-program-name))
	  
	  (setq gyach-buffer (current-buffer))
	  (gyach-mode)
;	  (run-hooks 'gyach-mode-hook)
)
	(pop-to-buffer gyach-buffer))
      (pop-to-buffer buffer))
  ;; configure our comint usage now that we have a buffer
  (set (make-local-variable 'comint-process-echoes) nil)
  (set (make-local-variable 'comint-input-sender) 'gyach-simple-send)
  (set (make-local-variable 'scroll-conservatively) 0)
  (set (make-local-variable 'comint-scroll-show-maximum-output) nil)

  (add-hook 'comint-preoutput-filter-functions 'gyach-preoutput-filter-functions nil t)
  (add-hook 'comint-output-filter-functions 'gyach-output-filter-functions nil t)
  (add-hook 'comint-input-filter-functions 'gyach-input-filter-functions nil t)

  (mapcar '(lambda (hook)
	     (remove-hook hook t t))
	  '(comint-preoutput-filter-functions 
	    comint-output-filter-functions 
	    comint-input-filter-functions))
  (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom t))


(defun gyach-match (pattern1 pattern2)
  (multiple-value-bind (matchp bindings)
      (match pattern1 pattern2)
    (if (and matchp (null bindings))
	t
      bindings)))

(defvar *gyach-message-patterns-alist*
  '((emote . (message :type emote
		      :room (*var* room) 
		      :who (*var* who) 
		      :message (*var* message)))
    (plain . (message :type plain 
		      :room (*var* room) 
		      :who (*var* who) 
		      :message (*var* message)))
    (enter . (message :type join 
		      :room (*var* room) 
		      :id (*var* id) 
		      :alias (*var* alias) 
		      :age (*var* age) 
		      :sex (*var* sex) 
		      :location (*var* location) 
		      :webcam (*var* webcam)))
    (leave . (message :type leave
		      :room (*var* room) 
		      :who (*var* who)))
    (login . (message :type login-response :sub-type (*var* sub-type) :message (*var* message)))))

(defun gyach-pattern (name)
  (cdr (assoc name *gyach-message-patterns-alist*)))

(defun base64-decode-list (list)
  (loop for e in list collect 
	(if (stringp e)
	    (base64-decode-string e)
	  e)))

(defun gyach-preoutput-filter-functions (string)
  "Clean up text before insertion"
  (let ((message (base64-decode-list (car (read-from-string string)))))
    (acond ((gyach-match (gyach-pattern 'plain) message)
	     (gyach-render-default (get-binding '(*var* who) it) (get-binding '(*var* message) it)))
	    ((gyach-match (gyach-pattern 'emote) message)
	     (gyach-render-action (get-binding '(*var* who) it) (get-binding '(*var* message) it)))
	    ((gyach-match (gyach-pattern 'leave) message)
	     (gyach-render-leave (get-binding '(*var* who) it)))
	    ((gyach-match (gyach-pattern 'enter) message)
	     (gyach-render-enter (get-binding '(*var* id) it)))
	    ((gyach-match (gyach-pattern 'login) message)
	     (gyach-render-login-response (get-binding '(*var* message) it)))
	    (t
	     (format "Ooops! Don't handle a %s yet.\n" message)))))

;; ;;   (dolist (h gyach-preoutput-hook)
;; ;;     (setq string (funcall h string))))
;; ;;   (cond ;; plain messages
;; ;;         ((string-match (concat "^\\(" gyach-username-regexp "\\): \\(.*\\)") string)
;; ;; 	 (let ((user (downcase (match-string 1 string)))
;; ;; 	       (text (match-string 2 string)))
;; ;; 	   ;; be very aggressive (don't rely on the room list package from yahoo)
;; ;; 	   (gyach-room-list-add user)
;; ;; 	   (if (gyach-is-ignorable user) "" (gyach-make-rendered-text 'default user text))))
;; ;; 	;; action messages
;; ;; 	((string-match (concat "^\\*\\ \\(" gyach-username-regexp "\\)\\(.*\\)") string)
;; ;; 	 (let ((user (downcase (match-string 1 string)))
;; ;; 	       (text (match-string 2 string)))
;; ;; 	   ;; be very aggressive (don't rely on the room list package from yahoo)
;; ;; 	   (gyach-room-list-add user)
;; ;; 	   (if (gyach-is-ignorable user) "" (gyach-make-rendered-text 'action user text))))
;; ;; 	;; leave/enter events
;; ;; 	((string-match (concat "^\\(" gyach-username-regexp "\\) \\(enters\\|leaves\\)") string)
;; ;; 	 (let ((user (downcase (match-string 1 string)))
;; ;; 	       (event (match-string 2 string))
;; ;; 	       (event-type))
;; ;; 	   (cond ((equal event "enters")
;; ;; 		  (setq event-type 'enter))
;; ;; 		 ((equal event "leaves")
;; ;; 		  (setq event-type 'leave)))
;; ;; 	   (gyach-process-events event-type user)
;; ;; 	   (if (not gyach-suppress-enter-leave-messages)
;; ;; 	       (gyach-make-rendered-text event-type user nil))))
;; ;; 	;; initialization messages
;; ;; 	((string-match (concat "^init room \\(.*\\)") string)
;; ;; 	 (let ((room-list (remove "" (split-string 
;; ;; 			   (replace-regexp-in-string "\\\\n" "" (downcase (match-string 1 string)))
;; ;; 			   ","))))
;; ;; 	   (dolist (user room-list)
;; ;; 	     (gyach-room-list-add user))
;; ;; 	   (gyach-make-rendered-text 'names nil room-list)))
;; ;; 	;; server messages
;; ;; 	((string-match "^\\*\\*\\* \\(.*\\)" string)
;; ;; 	 (gyach-make-rendered-text 'server nil (match-string 1 string)))
;; ;; 	;; private messages
;; ;; 	((string-match (concat "^\\(" gyach-username-regexp "\\) <private to " 
;; ;; 			       gyach-yahoo-username "> \\(.*\\)") 
;; ;; 		       string)
;; ;; 	 (let ((user (downcase (match-string 1 string)))
;; ;; 	       (text (match-string 2 string)))
;; ;; 	   (if (gyach-is-ignorable user) 
;; ;; 	       "" 
;; ;; 	     (gyach-make-rendered-text 'private user text))))
;; ;; 	(t 
;; ;; 	 string)))

(defun gyach-output-filter-functions (string)
  string)

(defun gyach-input-filter-functions (string)
  string)

;;   (dolist (h gyach-output-hook)
;;     ;; FIXME: shit what was i thinking?
;;     (funcall h comint-last-output-start (point))))

;; (defun gyach-input-filter-functions (string)
;;   "Intercept gyach.el command overrides."
;;   (cond ;; pass-through commands 
;; 	((string-match "^/\\(tell\\|think\\|quit\\|\\join\\)\\ +" string)
;; 	 (setq gyach-dont-send nil))
;; 	;; user-custom commands
;; 	((string-match "^/\\([a-zA-Z]+\\)\\ ?\\(.*\\)" string)
;; 	 (setq gyach-dont-send t
;; 	       gyach-command 'custom
;; 	       gyach-command-arg (cons (downcase (match-string 1 string)) (match-string 2 string))))
;; 	(t
;; 	  (setq gyach-dont-send nil))))

;; (defun gyach-custom-QUIT (proc arg)
;;   (kill-process proc)
;;   (gyach-save))

;; (define-key comint-mode-map [?\M-\t] 'ispell-complete-word)
;; (define-key comint-mode-map [?\t] 'gyach-complete)

;; ;; (add-to-list 'gyach-preoutput-hook 
;; ;;  	     'gyach-preoutput-url-sniff)

;; ;; (add-to-list 'gyach-output-hook
;; ;; 	     'gyach-output-fill)

(provide 'gyach)

;;; gyach.el ends here
