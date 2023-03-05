;;; doc-view-poppler.el --- use poppler for pdf->png in doc-view  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  OGAWA Hirofumi

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Keywords: pdf, doc-view

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

;; This is an alternative pdf->png converter for `doc-view' which is
;; using pdftocairo, instead of pdfdraw/mudraw.
;;
;; For example,
;;
;; (with-eval-after-load 'doc-view
;;   (require 'doc-view-poppler)
;;   (setq doc-view-pdf->png-converter-function
;;         #'doc-view-pdf->png-converter-poppler))

;;; Code:

(require 'doc-view)

(defvar doc-view-poppler-program "pdftocairo")

;;;###autoload
(defun doc-view-pdf->png-converter-poppler (pdf png page callback)
  "Convert PDF to PNG by using `doc-view-poppler-program'.
If PAGE is non-nil, convert only specified page.  CALLBACK is
callback for caller."
  (let* ((path (expand-file-name png))
         (dir (file-name-directory path))
         (name "poppler-page"))
    (doc-view-start-process
     "pdf->png" doc-view-poppler-program
     `("-png"
       "-r" ,(format "%d" (round doc-view-resolution))
       ,@(if page `("-f" ,(format "%d" page) "-l" ,(format "%d" page)))
       ,pdf
       ,(concat dir name))
     (lambda ()
       ;; rename temporary name to target name
       (let ((re (if page
                     (format "%s-0*%d\\.png" name page)
                   (concat name "-[0-9]+\\.png")))
             (case-fold-search nil))
         (save-match-data
           (dolist (file (directory-files dir t re))
             (when (string-match (concat name "-0*\\([0-9]+\\)\\.png") file)
               (let ((page-nr (string-to-number (match-string 1 file))))
                 (rename-file file (format png page-nr) t))))))
       (funcall callback)))))

(provide 'doc-view-poppler)
;;; doc-view-poppler.el ends here
