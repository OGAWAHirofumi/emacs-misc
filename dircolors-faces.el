;;; dircolors-faces.el --- make faces from dircolors  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: faces, dired

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

;; This is not perfectly emulate ls colors though, it should be enough
;; for normal usage.
;;
;; For example,
;;
;; (require 'dircolors-faces)
;; (dircolors-initialize)
;; (dircolors-get-face "su")
;; (dircolors-ext-get-face "jpg")
;;
;; and the helper to use for dired,
;;
;; (add-hook 'dired-load-hook
;;	  (lambda ()
;;	    (require 'dircolors-faces)
;;	    (setq dired-font-lock-keywords (dircolors-make-font-lock-keywords))
;;	    ))
;;
;; The dircolors types
;;
;; rs: RESET, no: NORMAL
;; fi: FILE,
;;     su: SETUID, sg: SETGID, ca: CAPABILITY, ex: EXEC, mh: MULTIHARDLINK
;; di: DIR
;;     tw: STICKY_OTHER_WRITABLE, ow: OTHER_WRITABLE, st: STICKY
;; ln: LINK
;;     or: ORPHAN, mi: MISSING
;; pi: FIFO, so: SOCK, bd: BLK, cd: CHR, do: DOOR

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'seq)
(require 'dired)
(require 'ansi-color)

(defgroup dircolors nil
  "Dircolors faces."
  :prefix "dircolors-"
  :version "25.1"
  :group 'dircolors)

(defcustom dircolors-use-dircolors t
  "If non-nil and LS_COLORS is NULL, run dircolors to get LS_COLORS."
  :type 'boolean
  :group 'dircolors)

(defcustom dircolors-use-bold nil
  "If non-nil, allow to use bold for dircolors faces.
If changed, you have to reinitialize tables by `dircolors-initialize'."
  :type 'boolean
  :group 'dircolors)

(defcustom dircolors-symlink-arrow-face nil
  "Face name used for arrow part of symbolic link line.
For example, `dired-symlink-face'."
  :type 'face
  :group 'dircolors)

(defvar dircolors-table (make-hash-table :test 'equal))
(defvar dircolors-ext-table (make-hash-table :test 'equal))

(defun dircolors-make-tables (ls-colors)
  "Parse LS-COLORS string, then add to maps."
  (let ((wsp split-string-default-separators)
	code-str color-str)
    (dolist (str (split-string ls-colors ":" t wsp))
      (when (string-match "\\([^=]+\\)=\\(.*\\)" str)
	(setq code-str (match-string 1 str))
	(setq color-str (match-string 2 str))
	(if (and (string= code-str "ln") (string= color-str "target"))
	    ;; ln=target is special case.
	    (puthash code-str color-str dircolors-table)
	  ;; convert like: "*.jpg=40;33;01" => ("jpg" (40 33 01))
	  ;; FIXME: LS_COLORS has more complex formats
	  (let (colors)
	    (dolist (num (split-string color-str ";" t wsp))
	      (setq num (string-to-number num 10))
	      (when (and (not dircolors-use-bold) (= 1 num))
		(setq num 0))
	      (unless (= num 0)		; 0 is error or none color
		(push num colors)))
	    (if (string-prefix-p "*." code-str)
		(puthash (substring code-str 2) (nreverse colors)
			 dircolors-ext-table)
	      (puthash code-str (nreverse colors) dircolors-table))))))))

(defun dircolors-get-face (code)
  "Return face match to dircolors CODE key."
  (save-match-data
    (ansi-color--find-face (gethash code dircolors-table))))
(defun dircolors-ext-get-face (ext)
  "Return face match to dircolors EXT key."
  (save-match-data
    (ansi-color--find-face (gethash ext dircolors-ext-table))))

(defun dircolors-run-dircolors ()
  "Get dircolors output for xterm."
  (shell-command-to-string
   "eval $(TERM=xterm dircolors -b) && echo $LS_COLORS"))

(defun dircolors-initialize ()
  "Initialize dircolors maps."
  (clrhash dircolors-table)
  (clrhash dircolors-ext-table)
  (let ((ls-colors (or (getenv "LS_COLORS")
		       (and dircolors-use-dircolors
			    (dircolors-run-dircolors)))))
    (and ls-colors (dircolors-make-tables ls-colors))))

;;
;; dired font-lock helpers
;;

(defconst dircolors-modes-code-table
  '(
    ;; STICKY_OTHER_WRITABLE type
    ("d[-r][-w].[-r][-w].[-r]w[tT]" "tw")
    ;; OTHER_WRITABLE type
    ("d[-r][-w].[-r][-w].[-r]w." "ow")
    ;; STICKY type
    ("d[-r][-w].[-r][-w].[-r].[tT]" "st")
    ;; SETUID type
    ("-[-r][-w][sS][-r][-w].[-r][-w]." "su")
    ;; SETGID type
    ("-[-r][-w].[-r][-w][sS][-r][-w]." "sg")
    ;; CAPABILITY is unsupported
    ))

(defconst dircolors-type-code-table '(("-" "fi") ("d" "di") ("p" "pi")
				      ("s" "so") ("b" "bd") ("c" "cd")))

;; make (match-string 1, 2, and 3) to match symlink source/target
(defun dircolors-match-symlink (limit)
  "Matcher for symlink by using `font-lock-keywords'.
1 is source filename, 2 is arrow, 3 is target filename in (match-string).
Argument LIMIT limits search."
  (let* ((start (dired-move-to-filename))
	 (end (dired-move-to-end-of-filename t))
	 source arrow target)
    (if (or (not start) (not end) (>= start end))
	;; no match
	nil
      ;; match symlink source
      (goto-char start)
      (re-search-forward ".+" end t)
      (setq source (match-data))
      ;; match arrow
      (re-search-forward " -> " limit t)
      (setq arrow (match-data))
      ;; match symlink target
      (re-search-forward ".+" limit t)
      (setq target (match-data))
      ;; set match-data with fake data for 0 pos
      (set-match-data (append '(nil nil) source arrow target))
      t)))

(defun dircolors-find-face (name)
  "Return dircolors face for NAME."
  (when (file-symlink-p name)
    (error "Symlink should be handled in `dircolors-get-symlink-face'"))
  (let* ((attrs (file-attributes name))
	 (modes (file-attribute-modes attrs)))
    (save-match-data
      (cond
       ((null modes)
	dired-symlink-face)

       ;; special modes are handled here
       ((when-let ((found (seq-find (lambda (x)
				      (string-match (nth 0 x) modes))
				    dircolors-modes-code-table)))
	  (dircolors-get-face (nth 1 found))))

       ;; EXEC type
       ((string-match dired-re-exe (concat "  " modes))
	(dircolors-get-face "ex"))
       ;; MULTIHARDLINK is unsupported

       ;; If not colored, based on file type (DIR/FIFO/SOCK/BLOCK/CHR)
       ;; DOOR is unsupported
       ((when-let ((found (seq-find (lambda (x)
				      (string-prefix-p (nth 0 x) modes))
				    dircolors-type-code-table)))
	  (dircolors-get-face (nth 1 found))))

       ;; extension base coloring
       ((string-match (concat ".+" (dircolors-ext-re)) name)
	(dircolors-ext-get-face (match-string 1 name)))

       (t
	dired-symlink-face)))))

(defun dircolors-get-target-symlink-face (name)
  "Return face for NAME based on type of dereferenced symlink."
  (let ((truename (file-truename name)))
    (dircolors-find-face truename)))

(defun dircolors-get-symlink-face (for-target)
  "Return face for symlink or symlink target.
If FOR-TARGET is non-nil, return the face for target filename.
Otherwise source filename."
  (save-match-data
    ;; Get correct name from dired, not regexp search.
    (let* ((name (dired-get-filename t t))
	   (exists (and name (file-exists-p name))) ; deref symlink
	   colors)
      ;; choice color by state of symlink
      (if for-target
	  (if exists
	      (setq colors "target")
	    (setq colors (or (gethash "mi" dircolors-table)
			     (gethash "or" dircolors-table))))
	(if exists
	    (setq colors (gethash "ln" dircolors-table))
	  (setq colors (or (gethash "or" dircolors-table)
			   (gethash "ln" dircolors-table)))))
      ;; "target" is coloring based on target type
      (if (and (stringp colors) (string= "target" colors))
	  (dircolors-get-target-symlink-face name)
	(ansi-color--find-face colors)))))

(defun dircolors-make-fmt-keyword (fmt code)
  "Make `font-lock-keywords' matcher for file type.
FMT is file type provided by ls, CODE is dircolors code."
  (list (concat dired-re-maybe-mark dired-re-inode-size fmt "[^:]")
	`(".+" (dired-move-to-filename) nil (0 (dircolors-get-face ,code)))))

(defun dircolors-make-modes-keyword (modes-re &optional code)
  "Make `font-lock-keywords' matcher for modes.
MODES-RE is modes provided by ls, CODE is dircolors code."
  (list (concat dired-re-maybe-mark dired-re-inode-size modes-re)
	`(".+" (dired-move-to-filename) nil (0 (dircolors-get-face ,code)))))

(defun dircolors-ext-re ()
  "Regexp for extensions."
  (let (extensions)
    (maphash (lambda (key _value) (push key extensions)) dircolors-ext-table)
    (concat "\\.\\(" (regexp-opt extensions) "\\)$")))

;; The order of this list is important to get proper fallback if
;; LS_COLORS didn't define code.
(defvar dircolors-font-lock-keywords
  `(
    ;; LINK/ORPHAN/MISSING type
    (,dired-re-sym
     (dircolors-match-symlink nil nil
			      (1 (dircolors-get-symlink-face nil))
			      (2 dircolors-symlink-arrow-face)
			      (3 (dircolors-get-symlink-face t))))

    ;; DIR are handled by dired basically
    ;; special modes are handled here
    ,@(mapcar (lambda (x)
		(dircolors-make-modes-keyword (car x) (cadr x)))
	      dircolors-modes-code-table)

    ;; EXEC type
    (,dired-re-exe (".+" (dired-move-to-filename) nil
		    (0 (dircolors-get-face "ex"))))
    ;; MULTIHARDLINK is unsupported

    ;; If not colored, based on file type (DIR/FIFO/SOCK/BLOCK/CHR)
    ;; DOOR is unsupported
    ,@(mapcar (lambda (x)
		(dircolors-make-fmt-keyword (car x) (cadr x)))
	      dircolors-type-code-table)
    ))

(defvar dircolors-ext-font-lock-keywords
  `(
    ;; Use eval to use latest dircolors tables.
    (eval .
	  ;; It is quicker to first find just an extension, then go
	  ;; back to the start of that file name.  So we do this
	  ;; complex MATCH-ANCHORED form.
	  (list (dircolors-ext-re)
		'(".+\\.\\(.*\\)" (dired-move-to-filename) nil
		  (0 (dircolors-ext-get-face (match-string 1))))))
    )
  )

(defun dircolors-insert-before (key alist &rest sequences)
  "At position before KEY on ALIST, insert SEQUENCES into.
Return inserted the result of alist.
The last argument may not copied, may used as the tail of the new list.

    (setq x1 \\='((\"foo\" 1) (\"bar\" 2) (\"baz\" 3)))
    (setq x2 \\='((\"foo1\" 4) (\"bar2\" 5)))
    (dircolors-insert-before \"bar\" x1 x2)
    => ((\"foo\" 1) (\"foo1\" 4) (\"bar2\" 5) (\"bar\" 2) (\"baz\" 3))"
  (if (equal key (caar alist))
      ;; Easy case, just append all
      (append (apply 'append sequences) alist)
    (when (assoc key alist)
      ;; Found key somewhere, find the position to insert
      (let* ((new-alist (copy-sequence alist))
	     (pos new-alist))
	(catch 'found
	  (while pos
	    (when (equal key (caar (cdr pos)))
	      (throw 'found t))
	    (setq pos (cdr pos))))
	(let ((after (cdr pos)))
	  (setcdr pos nil)
	  (append new-alist (apply 'append sequences) after))))))

(defvar dircolors-dired-font-lock-keywords dired-font-lock-keywords
  "Original `dired-font-lock-keywords'.")

;; This is fragile against modification of dired.el though, this would
;; be realistic and better off than copying from dired.el.
(defun dircolors-make-font-lock-keywords ()
  "Return `font-lock-keywords' can set to `dired-font-lock-keywords'."
  (dircolors-initialize)
  ;; Use `dired-font-lock-keywords' as template, and adds keywords at
  ;; proper position.
  (let ((keywords dircolors-dired-font-lock-keywords))
    ;; Insert keywords between "directory" and "mark" matchers.
    (when (< 0 (hash-table-count dircolors-table))
      (setq keywords (dircolors-insert-before
		      dired-re-dir keywords dircolors-font-lock-keywords)))
    ;; Add keywords at last, to match only if no match else.
    (when (< 0 (hash-table-count dircolors-ext-table))
      (setq keywords (append
		      keywords dircolors-ext-font-lock-keywords)))
    keywords))

(provide 'dircolors-faces)
;;; dircolors-faces.el ends here
