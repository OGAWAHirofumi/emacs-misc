;;; tab-bar-session.el --- Save session of tab-bar   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: convenience

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

(require 'desktop)
(require 'tab-bar)

(defgroup tbsession nil
  "Save status of Emacs when you exit."
  :group 'frames)

(defcustom tbsession-default-file-name
  (convert-standard-filename "tbsession-configs.el")
  "File used to save and load window configurations."
  :type 'file)

(defcustom tbsession-default-dirname user-emacs-directory
  "The directory in which the desktop file should be saved."
  :type 'directory)

(defcustom tbsession-line-style 'smart
  "Mode line style for tab-bar-session.

Possible values are:
  `smart' - hide when only one tab, otherwise use index.
  `index' - use index of tab-bar.
  nil     - hide always"
  :type '(choice (const :tag "Smart" smart)
		 (const :tag "Index" index)
		 (const :tag "Hide" nil)))

(defface tbsession-line-delimiters
  '((t (nil)))
  "Face for the mode line indicator delimiters.")

(defface tbsession-line-inactive
  '((t (nil)))
  "Face for the inactive items of the mode line indicator.")

(defface tbsession-line-active
  '((t (:inherit mode-line-emphasis)))
  "Face for the active items of the mode line indicator.")

(defcustom tbsession-line-left-delimiter "["
  "Left delimiter of the mode line indicator."
  :type 'string)

(defcustom tbsession-line-right-delimiter "]"
  "Right delimiter of the mode line indicator."
  :type 'string)

(defun tbsession-filter-alist ()
  "Return `frameset-filter-alist' only for tabs."
  (let ((frameset-filter-alist (copy-tree frameset-filter-alist))
	(save-alist '(tabs)))
    (mapc
     (lambda (element)
       (let* ((param (car element))
	      (action (if (memq param save-alist)
			  (cdr (assq param frameset-filter-alist))
			:never)))
	 (push (cons param action) frameset-filter-alist)))
     (frame-parameters))
    frameset-filter-alist))

(defun tbsession-save (dirname)
  "Save tab-bar session to `tbsession-default-dirname'.
With prefix argument \\[universal-argument], prompt for DIRNAME."
  (interactive (list
		(let ((default tbsession-default-dirname))
		  (if current-prefix-arg
                      (read-directory-name "Directory to save Session file in: "
					   default default t)
		    default))))
  (let ((desktop-base-file-name tbsession-default-file-name)
	;; Minimize variables to save desktop file
	(frameset-filter-alist (tbsession-filter-alist))
	(desktop-globals-to-save '())
	(desktop-locals-to-save '()))
    (desktop-save dirname)
    (message "Saved config to \"%s\""
	     (expand-file-name desktop-base-file-name dirname))))

(defun tbsession-load (&optional dirname)
  "Load tab-bar session from `tbsession-default-dirname'.
With prefix argument \\[universal-argument], prompt for DIRNAME."
  (interactive (list
		(let ((default tbsession-default-dirname))
		  (if current-prefix-arg
                      (read-directory-name "Directory to load Session file in: "
					   default default t)
		    default))))
  (let ((desktop-base-file-name tbsession-default-file-name))
    (desktop-read dirname)
    (message "Loaded config from \"%s\""
	     (expand-file-name desktop-base-file-name dirname))))

;;; mode-line

(defvar tbsession-mode t)

(defun tbsession-mode-line-indicator ()
  "Return a string representation of the window configurations."
  (let ((left-delimiter (propertize tbsession-line-left-delimiter
                                    'face 'tbsession-line-delimiters))
        (right-delimiter (propertize tbsession-line-right-delimiter
				     'face 'tbsession-line-delimiters))
	(line-style (if (and (eq tbsession-line-style 'smart)
			     (> (length (tab-bar-tabs)) 1))
			'index
		      nil))
        (current-index (tab-bar--current-tab-index)))
    (if (null line-style)
	""
      (concat left-delimiter
	      (propertize (number-to-string (1+ current-index))
			  'face 'tbsession-line-active)
	      right-delimiter))))

(defun tbsession-move-mode-line ()
  "Move mode line indicator to first."
  (let ((elt (assoc 'tbsession-mode mode-line-misc-info)))
    (setq mode-line-misc-info (delete elt mode-line-misc-info))
    (push '(tbsession-mode (:eval (tbsession-mode-line-indicator)))
	  mode-line-misc-info)))

(provide 'tab-bar-session)
;;; tab-bar-session.el ends here
