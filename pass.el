;;; pass.el --- password-store UI               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  OGAWA Hirofumi

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

(eval-when-compile
  (require 'rx))
(require 'text-property-search)
(require 'dired)

(defgroup pass nil
  "Support for pass major mode."
  :version "29.1"
  :group 'pass)

(defcustom pass-program "pass"
  "The default pass program."
  :type 'string)

(defcustom pass-editor-program "emacsclient"
  "The default editor program to edit."
  :type 'string)

(defcustom pass-store-dir (or (bound-and-true-p auth-source-pass-filename)
			      (getenv "PASSWORD_STORE_DIR")
			      "~/.password-store")
  "Get the directory of password-store."
  :type 'directory)

(defcustom pass-clip-timeout (string-to-number
			      (or (getenv "PASSWORD_STORE_CLIP_TIME") "45"))
  "Timeout seconds to clear saved text in kill-ring/clipboard."
  :type 'integer)

(defcustom pass-password-len (string-to-number
			      (or (getenv "PASSWORD_STORE_GENERATED_LENGTH")
				  "25"))
  "Default password length when generating."
  :type 'integer)

(defcustom qrcode-program "zbarimg"
  "The default QR code decoder."
  :type 'string)

(defcustom qrcode-program-options '("-q" "--raw")
  "Decode options for `qrcode-program'."
  :type '(repeat string))

(defvar-local pass-all-entries nil
  "Relative path for all entries.")

(defun pass-strip-name (name)
  "Strip .gpg from string NAME."
  (replace-regexp-in-string "\\.gpg\\'" "" name))

(defun pass-make-path (dir filename)
  "Normalize path to entry FILENAME for password-store.
DIR is path until FILENAME."
  (file-relative-name
   (file-name-concat dir (pass-strip-name filename))
   pass-store-dir))

(defun pass-face-entry (type name)
  "Make propertized string from NAME for file type TYPE."
  (let (face)
    (pcase type
      ('dir
       (setq face 'dired-directory))
      ('file
       (setq name (pass-strip-name name)))
      ('symlink
       (setq name (pass-strip-name name))
       (setq face 'dired-symlink))
      (_ (error "Unknown type: %s" type)))
    (if face
	(propertize name 'font-lock-face face)
      name)))

(defun pass-make-entry (type name dir)
  "Make entry from NAME for file type TYPE, and return string.
DIR is base directory to NAME."
  (let ((entry (pass-face-entry type name))
	(path (pass-make-path dir name)))
    (add-to-list 'pass-all-entries path)
    (propertize entry 'pass-path path 'pass-type type)))

(defun pass-make-symlink-entry (name target-name dir)
  "Make entry from NAME for symlink, and return string.
TARGET-NAME is the target of this symlink.  DIR is base directory to NAME."
  (let* ((realpath (file-name-concat dir name))
	 (dirp (file-directory-p realpath))
	 (target (pass-face-entry (if dirp 'dir 'file) target-name)))
    (concat (pass-make-entry 'symlink name dir) " -> " target)))

(defun pass-traverse-tree (dir &optional depth)
  "Traverse the password-store directory DIR.
DEPTH is the depth of current directory."
  (let ((depth (or depth 0)))
    (dolist (dirent (directory-files-and-attributes dir))
      (let* ((name (car dirent))
	     (attr (cdr dirent)))
	(unless (string-prefix-p "." name)
	  (insert (make-string (* 2 depth) ? ))
	  (let ((type (file-attribute-type attr)))
	    (if (eq t type)
		;; directory
		(progn
		  (insert (pass-make-entry 'dir name dir) "\n")
		  (pass-traverse-tree (file-name-concat dir name) (1+ depth)))
	      ;; non-directory
	      (if (null type)
		  ;; non-symlink
		  (insert (pass-make-entry 'file name dir))
		(insert (pass-make-symlink-entry name type dir)))
	      (insert "\n"))))))))

(defun pass-refresh-tree ()
  "Refresh password-store tree on current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Password Store\n")
    (setq pass-all-entries nil)
    (pass-traverse-tree pass-store-dir 1))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (pass-next-entry))

(defun pass-next-entry ()
  "Move cursor to next entry."
  (interactive nil pass-mode)
  (when-let ((prop (text-property-search-forward 'pass-path nil nil t)))
    (goto-char (prop-match-beginning prop))))

(defun pass-previous-entry ()
  "Move cursor to previous entry."
  (interactive nil pass-mode)
  (when-let ((prop (text-property-search-backward 'pass-path nil nil t)))
    (goto-char (prop-match-beginning prop))))

(defun pass-next-dirline ()
  "Move cursor to next directory entry."
  (interactive nil pass-mode)
  (when-let ((prop (text-property-search-forward 'pass-type 'dir t t)))
    (goto-char (prop-match-beginning prop))))

(defun pass-previous-dirline ()
  "Move cursor to previous directory entry."
  (interactive nil pass-mode)
  (when-let ((prop (text-property-search-backward 'pass-type 'dir t t)))
    (goto-char (prop-match-beginning prop))))

(defun pass-path-at-point ()
  "Get a path of entry at point."
  (let ((prop (save-excursion
		(beginning-of-line)
		(text-property-search-forward 'pass-path nil nil t))))
    (if (and prop (<= (prop-match-beginning prop) (line-end-position)))
	(prop-match-value prop)
      (user-error "No entry specified"))))

(defun pass-run-cmd (output &rest args)
  "Call pass command with arguments ARGS.
Output of process write to OUTPUT buffer."
  (apply #'call-process pass-program nil output nil args))

(defun pass-run-cmd-output (&rest args)
  "Call pass command with arguments ARGS, and write output to minibuffer."
  (with-temp-buffer
    (let ((status (apply #'pass-run-cmd t args)))
      (message "%s" (string-chop-newline (buffer-string)))
      status)))

(defun pass-pipe-cmd-output (string &rest args)
  "Call pass command with arguments ARGS, and write STRING to process input.
And write process output to minibuffer."
  (with-temp-buffer
    (let ((output (current-buffer)))
      (with-temp-buffer
	(insert string)
	(let ((status (apply #'call-process-region (point-min) (point-max)
			     pass-program nil output nil args)))
	  (with-current-buffer output
	    (message "%s" (string-chop-newline (buffer-string))))
	  status)))))

(defvar pass-read-history nil)

(defun pass-read-entry (prompt &optional initial-input)
  "Read entry from minibuffer.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially."
  (completing-read prompt pass-all-entries nil nil initial-input
		   'pass-read-history))

(defun pass-revert (&optional _arg _noconfirm)
  "Reread the Pass buffer."
  (let ((pos (point)))
    (pass-refresh-tree)
    (goto-char pos)
    (beginning-of-line)
    (pass-next-entry)
    (recenter)))

(defun pass-view-file (path)
  "View an entry PATH."
  (interactive (list (pass-path-at-point)) pass-mode)
  (let ((buffer (get-buffer-create (format "*%s*" path))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(let ((status (pass-run-cmd buffer "show" path)))
	  (unless (and (numberp status) (= 0 status))
	    (user-error "Failed view %s" path))
	  (goto-char (point-min))
	  (set-buffer-modified-p nil)))
      (view-buffer-other-window buffer nil 'kill-buffer))))

(defun pass-generate (target no-symbol &optional password-len)
  "Generate new password to TARGET.
If NO-SYMBOL is non-nil, a generated password doesn't use symbol chars.
PASSWORD-LEN is the length of new password (25 if nil)."
  (interactive
   (let* ((path (pass-path-at-point))
	  (initial-input (file-name-directory path))
	  (target (pass-read-entry "Generate password to: " initial-input))
	  (allow-symbol (y-or-n-p "Password allow symbol chars? "))
	  (len (read-number "Password length: " pass-password-len)))
     (list target (not allow-symbol) len))
   pass-mode)
  (let* ((password-len (or password-len pass-password-len))
	 (args `(,target ,(number-to-string password-len))))
    (when no-symbol
      (push "--no-symbols" args))
    (when (member target pass-all-entries)
      (when (y-or-n-p (format "%s exists.  Update password in place? " target))
	(push "--in-place" args)))
    (let ((status (apply #'pass-run-cmd-output "generate" args)))
      (when (and (numberp status) (= 0 status))
	(pass-revert)))))

(defvar pass-clip-last-save nil)
(defvar pass-clip-timer nil)

(defun pass-clear-last-save ()
  "Forget the saved data from `kill-ring' (and clipboard)."
  (when pass-clip-timer
    (cancel-timer pass-clip-timer)
    (setq pass-clip-timer nil))
  ;; clear clipboard and primary selection
  (when-let ((clip-target (car pass-clip-last-save))
	     (ring-target (cdr pass-clip-last-save)))
    (setq pass-clip-last-save nil)
    (when (equal clip-target (gui-get-selection 'PRIMARY))
      (let ((select-enable-primary t))
	(gui-select-text "")))
    (when (equal clip-target (gui-get-selection 'CLIPBOARD))
      (let ((select-enable-clipboard t))
	(gui-select-text "")))
    ;; clear kill-ring
    (setq kill-ring (delete ring-target kill-ring))
    (when (equal ring-target (car kill-ring-yank-pointer))
      (setq kill-ring-yank-pointer
	    (delete ring-target kill-ring-yank-pointer)))))

(defun pass-run-clear-timer (&optional timeout)
  "Run time to clear saved data.
Perform an action at time TIMEOUT seconds after."
  (let ((timeout (or timeout pass-clip-timeout)))
    (setq pass-clip-timer
	  (run-at-time timeout nil #'pass-clear-last-save))
    timeout))

(defun pass-clip-save (text)
  "Save data TEXT to `kill-ring' (and clipboard)."
  ;; clear old save
  (pass-clear-last-save)
  ;; save text
  (let ((select-enable-clipboard t))
    (kill-new text))
  (setq pass-clip-last-save `(,text . ,(car kill-ring-yank-pointer))))

(defun pass-entry-get-line (path linenum)
  "Read a line at line number LINENUM from path PATH."
  (with-temp-buffer
    (let ((status (pass-run-cmd t "show" path)))
      (unless (and (numberp status) (= 0 status))
	(user-error "Failed %s to read line at %d" path linenum))
      (goto-char (point-min))
      (when (= 0 (forward-line (1- linenum)))
	(buffer-substring (line-beginning-position) (line-end-position))))))

(defun pass-clip (path &optional linenum)
  "Copy a line at line number LINENUM in entry PATH to kill-ring/clipboard."
  (interactive
   (list (pass-path-at-point) (prefix-numeric-value current-prefix-arg))
   pass-mode)
  (setq linenum (or linenum 1))
  (if-let ((text (pass-entry-get-line path linenum)))
      (progn
	(pass-clip-save text)
	(let ((timeout (pass-run-clear-timer)))
	  (message "Copied %s to clipboard. Will clear in %d seconds."
		   path timeout)))
    (user-error "Can't read a target line at %d" linenum)))

(defun pass-edit-sentinel (proc _event)
  "Process sentinel for `pass-edit'.
PROC is process.  EVENT is process event."
  (unless (process-live-p proc)
    (let ((buffer (process-get proc 'buffer)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (goto-char (point-min))
	  (when (string= "emacsclient" (file-name-nondirectory
					pass-editor-program))
	    (forward-line 1)) 		; skip output from emacsclient
          (message "%s" (string-chop-newline
			 (buffer-substring (point) (point-max)))))))))

(defun pass-edit (path)
  "Edit an entry PATH."
  (interactive (list (pass-path-at-point)) pass-mode)
  (let ((buffer (get-buffer-create (format " *PassEdit %s*" path)))
	(process-connection-type t))
    (with-current-buffer buffer
      (erase-buffer)
      (with-environment-variables (("EDITOR" pass-editor-program))
	(let ((proc (start-process "pass-edit" buffer
				   pass-program "edit" path)))
	  (process-put proc 'buffer buffer)
	  (set-process-sentinel proc #'pass-edit-sentinel))))))

(defun pass-copy-path (path)
  "Copy an entry PATH to `kill-ring'."
  (interactive (list (pass-path-at-point)) pass-mode)
  (kill-new path)
  (message "%s" path))

(defun pass-copy-or-rename (cmd source target)
  "Copy or Rename SOURCE entry to TARGET entry.
CMD is a sub-command for pass (\"cp\" or \"mv\")."
  (when (string-empty-p target)
    (user-error "Target is an empty path"))
  (let ((status (pass-run-cmd-output cmd source target)))
    (when (and (numberp status) (= 0 status))
      (pass-revert))))

(defun pass-copy-or-rename-args (op-name)
  "Return a list of arguments for `pass-copy-or-rename'.
OP-NAME is a name of operation (\"Copy\" or \"Rename\")."
  (let* ((source (pass-path-at-point))
	 (name (file-name-nondirectory source))
	 (initial-input (file-name-directory source))
	 (target (pass-read-entry
		  (format "%s %s to: " op-name name) initial-input)))
    (list source target)))

(defun pass-copy (source target)
  "Copy an entry SOURCE to an entry TARGET."
  (interactive (pass-copy-or-rename-args "Copy") pass-mode)
  (pass-copy-or-rename "cp" source target))

(defun pass-rename (source target)
  "Rename an entry SOURCE to an entry TARGET."
  (interactive (pass-copy-or-rename-args "Rename") pass-mode)
  (pass-copy-or-rename "mv" source target))

(defvar pass-deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defun pass-remove (path)
  "Remove an entry PATH."
  (interactive (list (pass-path-at-point)) pass-mode)
  (let* ((name (file-name-nondirectory path))
	 (args (when (funcall pass-deletion-confirmer
			      (format "Delete %s? " name))
		 (if (file-directory-p (file-name-concat pass-store-dir path))
		     (when (funcall pass-deletion-confirmer
				    (format "Recursively delete %s? " name))
		       `("rm" "-r" ,path))
		   `("rm" ,path)))))
    (when args
      (let ((status (apply #'pass-run-cmd-output args)))
	(when (and (numberp status) (= 0 status))
	  (pass-revert))))))

(defun pass-otp-clip (path)
  "Copy a generated OTP code in an entry PATH to kill-ring/clipboard."
  (interactive (list (pass-path-at-point)) pass-mode)
  (with-temp-buffer
    (let ((status (pass-run-cmd t "otp" path))
	  (data (string-chop-newline (buffer-string))))
      (unless (and (numberp status) (= 0 status))
	(user-error "%s" data))
      (pass-clip-save data)
      (let ((timeout (pass-run-clear-timer)))
	(message "Copied %s OTP to clipboard. Will clear in %d seconds."
		 path timeout)))))

(defun pass-otp-append-string (path otpauth)
  "Append string OTPAUTH for otpauth URI to entry PATH."
  (let ((status (pass-pipe-cmd-output otpauth "otp" "append" path)))
    (when (and (numberp status) (= 0 status))
      (pass-revert))))

(defun pass-otp-append-uri (path otpauth)
  "Append otpauth URI OTPAUTH to an entry PATH."
  (interactive
   (list (pass-path-at-point) (read-string "otpauth URI: "))
   pass-mode)
  (pass-otp-append-string path otpauth))

(defun pass-decode-qrcode (file)
  "Decode QR code image file FILE."
  (let ((args `(,@qrcode-program-options ,(expand-file-name file))))
    (condition-case nil
	(car (apply #'process-lines qrcode-program args))
      (error
       (user-error "Failed to decode QR code: %s" file)))))

(defun pass-otp-append-qrcode (path img)
  "Decode the QR image file IMG for otpauth URI, then append to an entry PATH."
  (interactive
   (list (pass-path-at-point)
	 (read-file-name "QR code image file: " nil nil t))
   pass-mode)
  (let ((otpauth (pass-decode-qrcode img)))
    (pass-otp-append-string path otpauth)))

(defvar-keymap pass-mode-map
  :doc "Mode map used for `pass-mode'"
  "n"   #'pass-next-entry
  "p"   #'pass-previous-entry
  ">"   #'pass-next-dirline
  "<"   #'pass-previous-dirline
  "RET" #'pass-view-file
  "v"   #'pass-view-file
  "+"   #'pass-generate
  "c"   #'pass-clip
  "e"   #'pass-edit
  "w"   #'pass-copy-path
  "C"   #'pass-copy
  "R"   #'pass-rename
  "D"   #'pass-remove
  "o"   #'pass-otp-clip
  "O o" #'pass-otp-clip
  "O a" #'pass-otp-append-uri
  "O q" #'pass-otp-append-qrcode
  "g"   #'revert-buffer
  "q"   #'kill-current-buffer)

(define-derived-mode pass-mode special-mode "Pass"
  "A major mode for managing the password-store."
  :interactive nil
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq show-trailing-whitespace nil)
  (setq-local revert-buffer-function #'pass-revert))

;;;###autoload
(defun pass ()
  "Begin pass buffer to manage password-store."
  (interactive)
  (let ((buf (get-buffer-create "*Pass*")))
    (with-current-buffer buf
      (pass-mode)
      (pass-refresh-tree)
      (pop-to-buffer-same-window buf))))

(provide 'pass)
;;; pass.el ends here
