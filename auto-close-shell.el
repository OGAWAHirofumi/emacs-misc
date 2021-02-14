;;; auto-close-shell.el --- auto close shell when exit  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: convenience, processes

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

;; This makes a `shell' session, but windows are closed automatically
;; when shell process has exited.
;;
;; For example, use `auto-close-shell' command instead of shell
;;
;; M-x auto-close-shell
;;
;; or
;;
;; (autoload 'auto-close-shell "auto-close-shell" nil t)
;; (global-set-key "\C-cs" 'auto-close-shell)

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'tabulated-list)

(defgroup auto-close-shell nil
  "Auto close shell."
  :prefix "auto-close-shell-"
  :version "25.1"
  :group 'auto-close-shell)

(defcustom auto-close-shell-kill-buffer nil
  "If non-nil, kill buffer even if shell exited in background."
  :type 'boolean
  :group 'auto-close-shell)

(defvar auto-close-shell-buffers nil)
(defvar auto-close-shell-list-buffer "*Shell List*")

(defun auto-close-shell-list--refresh ()
  "Recompute the list of shell buffer list."
  (setq tabulated-list-entries nil)
  (dolist (buffer auto-close-shell-buffers)
    (let* ((bufname (buffer-name buffer))
	   (p (get-buffer-process buffer))
	   (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
	   (name (process-name p))
	   (dir (with-current-buffer buffer
		  (cond
		   ((buffer-file-name))
		   ((bound-and-true-p list-buffers-directory))
		   ((let ((dirname (and (boundp 'dired-directory)
					(if (stringp dired-directory)
					    dired-directory
					  (car dired-directory)))))
		      (and dirname (expand-file-name dirname)))))))
	   (status (symbol-name (process-status p)))
	   (info (format "(%s %s) %s" name status (abbreviate-file-name dir))))
      (push (list buffer (vector bufname pid info)) tabulated-list-entries)))
  (tabulated-list-init-header))

(defun auto-close-shell-list-update ()
  "Update a list in `auto-close-shell-list-buffer'."
  (when-let ((buffer (get-buffer auto-close-shell-list-buffer)))
    (with-current-buffer buffer
      (revert-buffer))))

(defun auto-close-shell-list-select ()
  "Visit shell buffer at current selected line."
  (interactive nil auto-close-shell-list-mode)
  (when-let ((buffer (tabulated-list-get-id)))
    (switch-to-buffer buffer)))

(defun auto-close-shell-list-shell ()
  "Create new shell buffer."
  (interactive nil auto-close-shell-list-mode)
  (ignore-errors
    (delete-window))
  (let ((current-prefix-arg '(4)))
    (call-interactively #'auto-close-shell)))

(defun auto-close-shell-list-quit ()
  "Quit shell list buffer."
  (interactive nil auto-close-shell-list-mode)
  (bury-buffer)
  (ignore-errors
    (delete-window)))

(defvar auto-close-shell-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m"	'auto-close-shell-list-select)
    (define-key map "s"		'auto-close-shell-list-shell)
    (define-key map "q"		'auto-close-shell-list-quit)
    map))

(define-derived-mode auto-close-shell-list-mode tabulated-list-mode "Shell List"
  "Major mode for shell buffer list."
  :interactive nil
  (setq tabulated-list-format [("Name" 12 t)
			       ("PID" 8 t)
			       ("Info" 40 t)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'auto-close-shell-list--refresh nil t))

;;;###autoload
(defun auto-close-shell-list ()
  "Display a list of all shell buffers."
  (interactive)
  (let ((buffer (get-buffer-create auto-close-shell-list-buffer)))
    (with-current-buffer buffer
      (auto-close-shell-list-mode)
      (auto-close-shell-list--refresh)
      (tabulated-list-print))
    ;; find window to display
    (if-let ((window (catch 'done
		       (dolist (b auto-close-shell-buffers)
			 (when-let ((w (get-buffer-window b)))
			   (throw 'done w))))))
	(progn
	  (select-window window)
	  (switch-to-buffer buffer))
      (pop-to-buffer buffer))))

(defun auto-close-shell--remember (buffer)
  "Remember shell buffer BUFFER."
  (add-to-list 'auto-close-shell-buffers buffer t)
  (auto-close-shell-list-update))

(defun auto-close-shell--forget (buffer)
  "Forget shell buffer BUFFER."
  (setq auto-close-shell-buffers (delete buffer auto-close-shell-buffers))
  ;; if remaining only one buffer, kill `auto-close-shell-list-buffer'
  (when-let ((buffer (and (= 1 (length auto-close-shell-buffers))
			  (get-buffer auto-close-shell-list-buffer))))
    (delete-windows-on buffer t)
    (kill-buffer buffer))
  (auto-close-shell-list-update))

(defun auto-close-shell-sentinel (process event)
  "When shell PROCESS exit, kill buffer and window.
PROCESS and EVENT are to used to call original sentinel."
  (let ((sentinel (process-get process 'auto-close-shell-original-sentinel))
	(buffer (process-buffer process)))
    (auto-close-shell--forget buffer)
    ;; call original sentinel
    (funcall sentinel process event)
    ;; start auto close
    (when (not (process-live-p process))
      ;; close windows first
      (if-let ((window (get-buffer-window buffer t)))
	  (progn
	    (delete-windows-on buffer t)
	    (kill-buffer buffer))
	;; kill buffer was not in window (killed in background)
	(and auto-close-shell-kill-buffer (kill-buffer buffer))))))

;;;###autoload
(defun auto-close-shell (&rest args)
  "Setup shell with ARGS arguments, then add sentinel chain to shell process."
  (interactive)
  (if (and (null current-prefix-arg) (> (length auto-close-shell-buffers) 1))
      (auto-close-shell-list)
    (let* ((buffer (call-interactively #'shell args))
	   (process (get-buffer-process buffer))
	   (sentinel (and process (process-sentinel process))))
      (when (and process (not (eq sentinel 'auto-close-shell-sentinel)))
	(process-put process 'auto-close-shell-original-sentinel sentinel)
	(set-process-sentinel process #'auto-close-shell-sentinel))
      (auto-close-shell--remember buffer)
      buffer)))

(provide 'auto-close-shell)
;;; auto-close-shell.el ends here
