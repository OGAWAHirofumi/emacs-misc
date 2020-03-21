;;; tracker-search.el --- tracker search  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'subr-x)
(require 'text-property-search)
(require 'dbus)
(require 'ansi-color)
(require 'dired-aux)

(defgroup tracker-search nil
  "Interface for the tracker search."
  :version "28.1"
  :group 'tools)

(defcustom tracker-search-details '(mime title snippet)
  "Verbose level of search result."
  :type '(repeat (choice (const :tag "MIME type" mime)
			 (const :tag "Title" title)
			 (const :tag "Document content" snippet))))

(defface tracker-path-face
  '((default :inherit shadow))
  "The face for the path in tracker result.")

(defface tracker-title-face
  '((default :inherit font-lock-keyword-face))
  "The face for the title in tracker result.")

(defface tracker-mime-face
  '((default :inherit shadow))
  "The face for the MIME in tracker result.")

(defcustom tracker-snippet-begin "\033[1m"
  "ANSI escape sequences to mark the start of search words."
  :type 'string)

(defcustom tracker-snippet-end "\033[0m"
  "ANSI escape sequences to mark the end of search words."
  :type 'string)

(defcustom tracker-snippet-ellipsis (if (char-displayable-p ?…) "…" "...")
  "The ellipsis character for the snippet of content."
  :type 'string)

(defcustom tracker-snippet-words 10
  "The number of words for the snippet of content."
  :type 'integer)

(defsubst tracker-dbus-call (method &rest args)
  "Call the tracker method METHOD with ARGS over dbus."
  (apply #'dbus-call-method
	 :session
	 "org.freedesktop.Tracker1"
	 "/org/freedesktop/Tracker1/Resources"
	 "org.freedesktop.Tracker1.Resources"
	 method args))

(defun tracker-escape-query (query)
  "Escape a QUERY to use as search words."
  (replace-regexp-in-string "\\(\"\\)" "\\\\\"" query))

(defun tracker-search-fts (query)
  "Return the result of tracker search for QUERY."
  (tracker-dbus-call
   "SparqlQuery"
   (concat "SELECT"
	   " tracker:coalesce(nie:url(?f), ?f)"
	   " nie:title(?f)"
	   " nie:mimeType(?f)"
	   " fts:snippet(?f"
	   ", \"" tracker-snippet-begin "\""
	   ", \"" tracker-snippet-end "\""
	   ", \"" tracker-snippet-ellipsis "\""
	   ", " (number-to-string tracker-snippet-words)
	   ")"
	   " WHERE {"
	   "  ?f fts:match \"" (tracker-escape-query query) "\" ."
	   "  ?f tracker:available true ."
	   "}")))

(defun tracker-file-path (url)
  "Convert URL to the path of file."
  (string-trim-left url "file://"))

(defvar-local tracker-search-query-string nil)

(defun tracker-insert-results (results)
  "Insert the result of tracker search RESULTS."
  (mapc (lambda (x)
	  (let* ((path-prefix "")
		 (detail-prefix "  ")
		 (path (propertize (tracker-file-path (nth 0 x))
				   'font-lock-face 'tracker-path-face
				   'tracker-path t))
		 (title (and (memq 'title tracker-search-details)
			     (propertize (if (string= (nth 1 x) "")
					     "No title"
					   (nth 1 x))
					 'font-lock-face 'tracker-title-face
					 'tracker-title t)))
		 (mime (and (memq 'mime tracker-search-details)
			    (nth 2 x)
			    (propertize (nth 2 x)
					'font-lock-face 'tracker-mime-face
					'tracker-mime t)))
		 (snippet (and (memq 'snippet tracker-search-details)
			       (nth 3 x)
			       (propertize
				(mapconcat (lambda (x)
					     (concat detail-prefix x "\n"))
					   (split-string
					    (ansi-color-apply (nth 3 x))
					    "\n")
					   "")
				'tracker-snippet t))))
	    (insert path-prefix path "\n")
	    (when mime
	      (insert detail-prefix mime "\n"))
	    (when title
	      (insert detail-prefix title "\n"))
	    (when snippet
	      (insert snippet "\n"))))
	results))

(defun tracker-result-find-prop (pos prop &optional n)
  "Find the start position of PROP near the position POS.
If N is positive, N times next.  If N is negative N times previous."
  (or n (setq n 0))
  (save-excursion
    (let ((match t))
      (goto-char pos)
      (when (get-text-property pos prop)
	(when-let ((prev (previous-single-property-change pos prop)))
	  (goto-char prev))
	(setq match (text-property-search-forward prop t t nil)))
      (while (and match (/= n 0))
	(setq match (if (> n 0)
			(text-property-search-forward prop t t t)
		      (text-property-search-backward prop t t t)))
	(setq n (if (> n 0)
		    (1- n)
		  (1+ n))))
      (if (prop-match-p match)
	  match
	nil))))

(defun tracker-result-prev (arg)
  "Move cursor to a previous search result ARG times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((range (tracker-result-find-prop (point) 'tracker-path (- arg))))
    (when range
      (goto-char (prop-match-beginning range))
      (recenter))))

(defun tracker-result-next (arg)
  "Move cursor to a next search result ARG times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((range (tracker-result-find-prop (point) 'tracker-path arg)))
    (when range
      (goto-char (prop-match-beginning range))
      (recenter))))

(defun tracker-file-path-at-point (pos)
  "Return the path of result for position POS."
  (let ((range (or (tracker-result-find-prop pos 'tracker-path 0)
		   (tracker-result-find-prop pos 'tracker-path -1))))
    (when (null range)
      (user-error "Tracker result is not found"))
    (string-trim-left
     (buffer-substring (prop-match-beginning range) (prop-match-end range))
     " +")))

(defun tracker-result-dired ()
  "In tracker result, visit the directory that contain the this result."
  (interactive)
  (dired (file-name-directory (tracker-file-path-at-point (point)))))

(defun tracker-result-find-file ()
  "In tracker result, visit the file or directory named on this result."
  (interactive)
  (find-file (tracker-file-path-at-point (point))))

(defun tracker-result-find-file-other-window ()
  "In tracker result, visit this file or directory in another window."
  (interactive)
  (find-file-other-window (tracker-file-path-at-point (point))))

(defun tracker-result-run-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND for this result.
If no files are marked or a numeric prefix arg is given, the next
ARG files are used.  Just \\[universal-argument] means the
current file.  The prompt mentions the file(s) or the marker, as
appropriate.

In a noninteractive call (from Lisp code), you must specify
the list of file names explicitly with the FILE-LIST argument."
  (interactive
   (let ((files (list (tracker-file-path-at-point (point)))))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "! on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (dired-run-shell-command
   (dired-shell-stuff-it command file-list nil arg)))

(defvar tracker-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p"	'tracker-result-prev)
    (define-key map "n"	'tracker-result-next)
    (define-key map "!"	'tracker-result-run-shell-command)
    (define-key map "d" 'tracker-result-dired)
    (define-key map "e" 'tracker-result-find-file)
    (define-key map "f" 'tracker-result-find-file)
    (define-key map "\C-m" 'tracker-result-find-file)
    (define-key map "o" 'tracker-result-find-file-other-window)
    map)
  "Keymap used in `tracker-result-mode'.")

(define-derived-mode tracker-result-mode special-mode "Tracker-Result"
  ""
  (setq buffer-auto-save-file-name nil
	mode-line-buffer-identification
	(list (default-value 'mode-line-buffer-identification)
	      " {" 'tracker-search-query-string "}")
	truncate-lines t)
  (auto-fill-mode -1)
  (goto-char (point-min)))

;;;###autoload
(defun tracker-search (query)
  "Search QUERY text by tracker full text search."
  (interactive "sTracker Search: ")
  (let ((results (tracker-search-fts query))
	(buffer (get-buffer-create "*Tracker Result*")))
    (if (null results)
	(message "No hit: %s" query)
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
	  (buffer-disable-undo)
	  (erase-buffer)
	  (tracker-insert-results results)
	  (set-buffer-modified-p nil)
	  (tracker-result-mode)
	  (setq-local tracker-search-query-string query)))
      (select-window (display-buffer buffer)))))

(provide 'tracker-search)
;;; tracker-search.el ends here
