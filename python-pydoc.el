;;; python-pydoc.el --- pydoc helper for python-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: help, python

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
;; (add-hook 'python-mode-hook
;;	  (lambda ()
;;	    (autoload 'python-pydoc "python-pydoc" nil t)
;;	    (define-key python-mode-map "\C-ho" 'python-pydoc)))

;;; Code:

(require 'man)
(require 'python)

(defgroup python-pydoc nil
  "Python pydoc."
  :prefix "python-pydoc-"
  :version "25.1"
  :group 'python-pydoc)

(defcustom python-pydoc-program "pydoc"
  "Program used by `python-pydoc' to produce help."
  :type 'string
  :group 'python-pydoc)

(defvar python-pydoc-history nil
  "`python-pydoc' read history.")

;;;###autoload
(defun python-pydoc (symbol)
  "Run `pydoc' on WORD."
  (interactive
   (let ((default (python-eldoc--get-symbol-at-point)))
     (list (read-string (format "Describe symbol%s: "
				(if default
				    (format "(default %s)" default)
				  ""))
                        nil 'python-pydoc-history default))))
  (when (string= symbol "")
    (user-error "No pydoc args given"))
  (let ((Man-switches "")
	(manual-program python-pydoc-program))
    (Man-getpage-in-background symbol)))

;;;###autoload
(defun python-pydoc-at-point ()
  "Run a `perldoc' on the word around point."
  (interactive)
  (python-pydoc (python-eldoc--get-symbol-at-point)))

(provide 'python-pydoc)
;;; python-pydoc.el ends here
