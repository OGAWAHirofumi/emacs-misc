;;; temp-compile.el --- yet another compile extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: tools, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For example,
;;
;; (setq compilation-environment '("LANG=C"))
;; (setq compile-command "make ")
;; (autoload 'temp-compile "temp-compile" nil t)
;; (global-set-key [f5] 'temp-compile)
;;
;; Also see `temp-compile-conversion-alist' and `temp-compile-template-alist'
;; to know the way to choose compile commands.  Chosen commands are
;; added to compile history.

;;; Code:

(require 'compile)
(require 'dired)

(defgroup temp-compile nil
  "Template base `compile' extension."
  :prefix "temp-compile-"
  :version "25.1"
  :group 'temp-compile)

(defcustom temp-compile-ask-new-buffer t
  "If compile is still running, create new compile buffer.
If you are setting non-nil to `compilation-buffer-name-function',
this has no effect."
  :type 'boolean
  :version "25.1"
  :group 'temp-compile)

(defvar temp-compile-conversion-alist
  '(
    ("%c" . (file-relative-name default-directory))
    ("%C" . default-directory)
    ("%d" . (and buffer-file-name (file-relative-name
				   (file-name-directory buffer-file-name))))
    ("%D" . (and buffer-file-name (file-name-directory buffer-file-name)))
    ("%f" . (and buffer-file-name (file-relative-name buffer-file-name)))
    ("%F" . buffer-file-name)
    ("%e" . (and buffer-file-name (file-name-sans-extension
				   (file-relative-name buffer-file-name))))
    ("%E" . (and buffer-file-name (file-name-sans-extension buffer-file-name)))
    )
  "Alist of converters used for `temp-compile-template-alist'.
Each element is (KEY . VALUE).  KEY is string of the conversion
specifier to replace by the result of VALUE.  VALUE is cons,
function, symbol, or string to provide replacer string.")

(defvar temp-compile-template-alist
  '(
    ;; Candidate from file/buffer-local command
    (t . (if (local-variable-p 'compile-command) compile-command))
    ;; Candidate from state
    (t . (if makefile-p "make "))
    ("*.l" . (if (not makefile-p) "flex -d -o %e.c %f"))
    ("*.y" . (if (not makefile-p) "bison -t -o %e.c %f"))
    (asm-mode . (if (not makefile-p) "gcc -g -Wall -W -o %e %f"))
    (c-mode . (if (not makefile-p) '("gcc -g -Wall -W -o %e %f"
				     "gcc -g -O2 -Wall -W -o %e %f")))
    (c++-mode . (if (not makefile-p) '("g++ -g -Wall -W -o %e %f"
				       "g++ -g -O2 -Wall -W -o %e %f")))
    (java-mode . (if (not makefile-p) "javac -g %f"))
    (perl-mode . "perl -wc %f")
    (cperl-mode . "perl -wc %f")
    (sh-mode . (concat (if (eq sh-shell 'bash) "bash" "sh") " -n %f"))
    (emacs-lisp-mode
     . (if (not makefile-p) "emacs -batch -Q -L . -f batch-byte-compile %f"))
    )
  "Commands template to use by `temp-compile'.
Each element is (KEY . VALUE).  KEY is t, symbol to compare with
`major-mode', string to match filename by using `dired-glob-regexp',
or cons to check match.  VALUE provides compile string.

In cons, it can use the special value makefile-p to check if
makefile is exists for convenience.

If elements matched and provide compile string, those are
inserted to history to be used by `compile'.")

;;
;; Template helpers
;;

(defun temp-compile-eval (msg element &optional lexical)
  (let* ((value (cdr element))
	 (result (cond ((consp value)
			(eval value lexical))
		       ((functionp value)
			(funcall value))
		       ((symbolp value)
			(symbol-value value))
		       ((stringp value)
			value)
		       (t
			(user-error "Invalid %s: %s" msg element)))))
    ;; Make sure values are string
    (mapc (lambda (r)
	    (when (and r (not (stringp r)))
	      (user-error "Invalid result of %s: %s" msg r)))
	  (if (listp result) result (list result)))
    result))

(defun temp-compile-expand (line)
  "Expend LINE conversion specifiers by using `temp-compile-conversion-alist'."
  (let* ((percent-fmt '("%%" . "%"))
	 (alist (append (list percent-fmt) temp-compile-conversion-alist))
	 (case-fold-search nil)
	 (pos 0))
    (catch 'break
      (while (and (< pos (length line)) (setq pos (string-match "%." line pos)))
	(let* ((msg "conversion")
	       (key (match-string 0 line))
	       (converter (assoc-string key alist))
	       (result (if (null converter)
			   (user-error "Invalid %s key: \"%s\"" msg key)
			 (temp-compile-eval msg converter))))
	  (when (null result)
	    (setq line nil)
	    (throw 'break nil))
	  (setq line (replace-match result t nil line))
	  (setq pos (+ pos (length result))))))
    line))

(defun temp-compile-template-eval (template lexical)
  "Evaluate TEMPLATE with LEXICAL environment, then return result."
  (let ((key (car template)))
    (when (cond ((symbolp key)
		 (or (eq key t) (eq key major-mode)))
		((consp key)
		 (eval key lexical))
		((stringp key)
		 (let ((file (and buffer-file-name
				  (file-name-nondirectory buffer-file-name))))
		   (and file (string-match-p (dired-glob-regexp key) file))))
		(t
		 (user-error "Invalid template key: %s" key)))
      (temp-compile-eval "template" template lexical))))

(defun temp-compile-makefile-p ()
  "Return t if Makefile is exists."
  (or (file-exists-p "Makefile")
      (file-exists-p "GNUmakefile")
      (file-exists-p "makefile")))

(defun temp-compile-commands ()
  (let* ((makefile-p (temp-compile-makefile-p))
	 (lexical `((makefile-p . ,makefile-p)))
	 results)
    ;; Make matched list of template values
    (mapc (lambda (temp)
	    (let ((r (temp-compile-template-eval temp lexical)))
	      (if (listp r)
		  (setq results (append results r))
		(add-to-list 'results r t))))
	  temp-compile-template-alist)
    ;; Expand conversion specifiers
    (delq nil (mapcar #'temp-compile-expand results))))

;;
;; Buffer management
;;

;; FIXME: should remember buffer-local latest compile buffer?
(defvar temp-compile-buffers nil)

(defun temp-compile-buffer-add (buffer)
  (when temp-compile-ask-new-buffer
    ;; Update LRU list
    (setq temp-compile-buffers
	  (cons buffer (delete buffer temp-compile-buffers))))
  buffer)

(defun temp-compile-filter (fn sequence)
  (delq nil (mapcar (lambda (x) (and (funcall fn x) x)) sequence)))

(defun temp-compile-running-buffer-p (buffer)
  (when buffer
    (let ((proc (get-buffer-process buffer)))
      (when (and proc
		 (eq (process-status proc) 'run)
		 (not (eq (process-query-on-exit-flag proc) nil)))
	buffer))))
(defun temp-compile-running-buffers ()
  (temp-compile-filter #'temp-compile-running-buffer-p temp-compile-buffers))
(defun temp-compile-not-running-buffers ()
  (temp-compile-filter (lambda (x) (not (temp-compile-running-buffer-p x)))
		       temp-compile-buffers))

(defun temp-compile-buffer-name (name-of-mode)
  (save-match-data
    ;; Remove non live buffers
    (setq temp-compile-buffers
	  (temp-compile-filter #'buffer-live-p temp-compile-buffers))
    ;; Choice buffer name is not running, or new buffer name
    (let ((name (concat "*" (downcase name-of-mode) "*"))
	  (running-buffers (temp-compile-running-buffers))
	  (not-running-buffers (temp-compile-not-running-buffers)))
      (if (and running-buffers
	       (save-excursion
		 (save-window-excursion
		   (switch-to-buffer-other-window (car running-buffers) t)
		   (not (y-or-n-p
			 (format "A %s process is running; create new buffer? "
				 name-of-mode))))))
	  ;; Uses running buffer, so pop-up choose buffer
	  (buffer-name (car running-buffers))
	;; Uses non running buffer
	(if not-running-buffers
	    (buffer-name (car not-running-buffers))
	  (generate-new-buffer-name name))))))

(defvar-local temp-compile-command nil)

;;;###autoload
(defun temp-compile ()
  "Run `compile' with template based commands history."
  (interactive)
  (let ((commands (temp-compile-commands))
	(compilation-buffer-name-function compilation-buffer-name-function)
	(history-delete-duplicates t)	; cleanup duplicate histories
	buffer)
    (when (and temp-compile-ask-new-buffer
	       (null compilation-buffer-name-function))
      (setq compilation-buffer-name-function #'temp-compile-buffer-name))
    (if (null commands)
	(when (setq buffer (call-interactively #'compile))
	  (setq temp-compile-command compile-command))
      ;; Template base compile. Doesn't make sense to remember
      ;; template commands, so make `compile-history' local bind.
      (let ((compile-command (or temp-compile-command (car commands)))
	    (compile-history (append commands compile-history)))
	(when (setq buffer (call-interactively #'compile))
	  (setq temp-compile-command compile-command)))
      ;; After local bind of `compile-history', add user command to history
      (when (not (member temp-compile-command commands))
	(add-to-history 'compile-history temp-compile-command)))
    ;; Remember compile buffer
    (temp-compile-buffer-add buffer)))

(provide 'temp-compile)
;;; temp-compile.el ends here
