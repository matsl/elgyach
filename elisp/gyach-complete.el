;;; gyach-complete.el --- ElGyach command and nick completion

;; Copyright (C) 2001, 2002  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcCompletion

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file is obsolete.  Use completion from erc-pcomplete instead.
;; This file is based on hippie-expand, while the new file is based on
;; pcomplete.  There is no autoload cookie in this file.  If you want
;; to use the code in this file, add the following to your ~/.emacs:

;; (autoload 'erc-complete "erc-complete" "Complete nick at point." t) 


;; I ripped this right out of ERC.  Thanks to the original author,
;; Alex you rock!

;;; Code:


(require 'hippie-exp)

(defun gyach-complete ()
  "Complete nick at point.
See `gyach-try-complete-nick' for more technical info."
  (interactive)
  (let ((hippie-expand-try-functions-list '(gyach-try-complete-nick)))
    (hippie-expand nil)))

(defcustom gyach-nick-completion 'all
  "Determine how the list of nicks is determined during nick completion.
See `gyach-complete-nick' for information on how to activate this.

pals:   Use `gyach-pals'.
all:    All channel members.

You may also provide your own function that returns a list of completions.
One example is `gyach-nick-completion-exclude-myself',
or you may use an arbitrary lisp expression."
  :type '(choice (const :tag "List of pals" pals)
		 (const :tag "All channel members" all)
		 (const :tag "All channel members except yourself"
			gyach-nick-completion-exclude-myself)
		 (repeat :tag "List" (string :tag "Nick"))
		 function
		 sexp)
  :group 'gyach)

(defcustom gyach-nick-completion-ignore-case t
  "*Non-nil means don't consider case significant in nick completion.
Case will be automatically corrected when non-nil.
For instance if you type \"dely TAB\" the word completes and changes to
\"delYsid\"."
  :group 'gyach
  :type 'boolean)

(defun gyach-nick-completion-exclude-myself ()
  "Get a list of all the channel members except you.

This function returns a list of all the members in the channel, except
your own nick.  This way if you're named foo and someone is called foobar,
typing \"f o TAB\" will directly give you foobar.  Use this with
`gyach-nick-completion'."
  (delete
   gyach-yahoo-username
   (gyach-room-list)))

(defcustom gyach-nick-completion-postfix ": "
  "*When `gyach-complete' is used in the first word after the prompt,
add this string when a unique expansion was found."
  :group 'gyach
  :type 'string)

(defun gyach-command-list ()
  "Returns a list of strings of the defined user commands."
  (let ((case-fold-search nil))
    (mapcar (lambda (x)
	      (concat "/" (downcase (substring (symbol-name x) 13))))
	    (apropos-internal "gyach-custom-[A-Z]+"))))

(defun gyach-try-complete-nick (old)
  "Complete nick at point.
This is a function to put on `hippie-expand-try-functions-list'.
Then use \\[hippie-expand] to expand nicks.
The type of completion depends on `gyach-nick-completion'."
  (cond ((eq gyach-nick-completion 'pals)
	 (try-complete-gyach-nick old (gyach-pals)))
	((eq gyach-nick-completion 'all)
	 (try-complete-gyach-nick old (append
				       (gyach-room-list)
				       (gyach-command-list))))
	((functionp gyach-nick-completion)
	 (try-complete-gyach-nick old (funcall gyach-nick-completion)))
	(t
	 (try-complete-gyach-nick old gyach-nick-completion))))

(defvar try-complete-gyach-nick-window-configuration nil
  "The window configuration for `try-complete-gyach-nick'.
When called the first time, a window config is stored here,
and when completion is done, the window config is restored
from here.  See `try-complete-gyach-nick-restore' and
`try-complete-gyach-nick'.")

(defun try-complete-gyach-nick-restore ()
  "Restore window configuration."
  (if (not try-complete-gyach-nick-window-configuration)
      (when (get-buffer "*Completions*")
	(delete-windows-on "*Completions*"))
    (set-window-configuration
     try-complete-gyach-nick-window-configuration)
    (setq try-complete-gyach-nick-window-configuration nil)))

(defun try-complete-gyach-nick (old completions)
  "Try to complete current word depending on `gyach-try-complete-nick'.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise.  The
second argument COMPLETIONS is a list of completions to use.  Actually,
it is only used when OLD is nil.  It will be copied to `he-expand-list'
on the first call.  After that, it is no longer used.
Window configurations are stored in
`try-complete-gyach-nick-window-configuration'."
  (let (expansion
	final
	(alist (if (consp (car completions))
		   completions
		 (mapcar (lambda (s)
			   (if (and (gyach-complete-at-prompt)
				    (and (not (= (length s) 0))
					 (not (eq (elt s 0) ?/))))
			       (list (concat s gyach-nick-completion-postfix))
			     (list (concat s " "))))
			 completions))) ; make alist if required
	(completion-ignore-case gyach-nick-completion-ignore-case))
    (he-init-string (he-dabbrev-beg) (point))
    ;; If there is a string to complete, complete it using alist.
    ;; expansion is the possible expansion, or t.  If expansion is t
    ;; or if expansion is the "real" thing, we are finished (final is
    ;; t).  Take care -- expansion can also be nil!
    (unless (string= he-search-string "")
      (setq expansion (try-completion he-search-string alist)
	    final (or (eq t expansion)
		      (and expansion
			   (eq t (try-completion expansion alist))))))
    (cond ((not expansion)
	   ;; There is no expansion at all.
	   (try-complete-gyach-nick-restore)
	   (he-reset-string)
	   nil)
	  ((eq t expansion)
	   ;; The user already has the correct expansion.
	   (try-complete-gyach-nick-restore)
	   (he-reset-string)
	   t)
	  ((and old (string= expansion he-search-string))
	   ;; This is the second time around and nothing changed,
	   ;; ie. the user tried to expand something incomplete
	   ;; without making a choice -- hitting TAB twice, for
	   ;; example.
	   (try-complete-gyach-nick-restore)
	   (he-reset-string)
	   nil)
	  (final
	   ;; The user has found the correct expansion.
	   (try-complete-gyach-nick-restore)
	   (he-substitute-string expansion)
	   t)
	  (t
	   ;; We found something but we are not finished.  Show a
	   ;; completions buffer.  Substitute what we found and return
	   ;; t.
	   (setq try-complete-gyach-nick-window-configuration
		 (current-window-configuration))
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list (all-completions he-search-string alist)))
	   (he-substitute-string expansion)
	   t))))

(defun gyach-bol ()
  (beginning-of-line))

(defun gyach-at-beginning-of-line-p (point &optional bol-func)
  (save-excursion
    (funcall (or bol-func
		 'gyach-bol))
    (equal point (point))))

(defun gyach-complete-at-prompt ()
  "Returns t if point is directly after `gyach-prompt' when doing completion."
  (gyach-at-beginning-of-line-p (he-dabbrev-beg)))

(provide 'gyach-complete)

;;; gyach-complete.el ends here
