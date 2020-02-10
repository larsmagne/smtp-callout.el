;;; smtp-callout.el --- Verifying email addresses by doing an SMTP callout

;; Copyright (C) 2020 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: movies

;; smtp-callout.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; smtp-callout.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Usage:
;;
;; (smtp-callout "foo@example.org" :from-mail "me@my.mta")

;;; Code:

(require 'dns)

(define-error 'smtp-error "SMTP error")

(cl-defun smtp-callout (address
			&key (timeout 5)
			(helo-host (system-name))
			(from-mail user-mail-address)
			(debug nil))
  "Check whether ADDRESS is valid by doing an SMTP callout.
Returns nil if the address is valid and the error string if not.

TIMEOUT is a timeout in seconds.

HELO-HOST is the host that you will greet the SMTP server with.

FROM-MAIL is an email address that you will pretend to be sending
mail from."
  (condition-case err
      (smtp-callout--check-mail-address address timeout helo-host from-mail
					debug)
    (smtp-error (cdr err))))

(defun smtp-callout--check-mail-address (address timeout helo-host from-mail
						 debug)
  (with-temp-buffer
    (let* ((host (cadr (split-string address "@")))
	   (start (point))
	   line)
      ;; Resolve CNAME first.
      (setq host (or (dns-query host 'CNAME) host))
      ;; The the MTA's name.
      (setq server (or (cdr (dns-query host 'MX)) host))
      (let ((start-time (float-time))
	    (smtp (ignore-errors
		    (make-network-process :buffer (current-buffer)
					  :host server
					  :name "check"
					  :nowait t
					  :service 25))))
	(set-process-query-on-exit-flag smtp nil)
	(unless smtp
	  (signal 'smtp-error (format "Unable to contact %s" server)))
	;; Setting up the TCP connection to the SMTP server may time out.
	(while (and (< (- (float-time) start-time) timeout)
		    (eq (process-status smtp) 'connect))
	  (sleep-for 0.1))
	(when (eq (process-status smtp) 'connect)
	  (signal 'smtp-error (format "Timed out while contacting %s" server)))
	;; Read the initial greeting from the SMTP server.
	(smtp-callout--check smtp start timeout debug
			     "^220" "Initial greeting: %s")
	;; Send a HELO.
	(setq start (point-max))
	(process-send-string smtp (format "HELO %s\r\n" helo-host))
	(smtp-callout--check smtp start timeout debug
			     "^2" "HELO response: %s")
	;; Check whether the server accepts mail from us.
	(setq start (point-max))
	(process-send-string smtp (format "MAIL FROM:<%s>\r\n" from-mail))
	(smtp-callout--check smtp start timeout debug
			     "^2" "MAIL FROM response: %s")
	;; Check whether the server accepts mail to the address.
	(setq start (point-max))
	(process-send-string smtp (format "RCPT TO:<%s>\r\n" address))
	(smtp-callout--check smtp start timeout debug
			     "^2" "RCPT response: %s")
	(when debug
	  (message "%s" (buffer-string)))
	(process-send-string smtp "QUIT\r\n")
	(delete-process smtp)))))

(defun smtp-callout--check (smtp start timeout debug check message)
  (let ((line (smtp-callout--wait smtp start timeout)))
    (when (or (not line)
	      (not (string-match check line)))
      (when debug
	(message "%s" (buffer-string)))
      (signal 'smtp-error (format message (or line "(timed out while talking to server)"))))))

(defun smtp-callout--wait (process start timeout)
  (let ((start-time (float-time))
	found)
    (while (and (goto-char start)
		(not (setq found (re-search-forward "^[0-9]+ .*\n" nil t)))
		(< (- (float-time) start-time) timeout)
		(process-live-p process))
      (accept-process-output process 0.1 nil t))
    (and found
	 (string-trim
	  (replace-regexp-in-string "[\r\n]+" " "
				    (buffer-substring start (point-max)))))))

(provide 'smtp-callout)

;;; smtp-callout.el ends here
