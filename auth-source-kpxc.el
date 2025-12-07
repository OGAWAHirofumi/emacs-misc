;;; auth-source-kpxc.el --- Integrate auth-source with keepassxc -*- lexical-binding: t -*-

;; Copyright (C) 2025 OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>

;; Author: OGAWA Hirofumi <hirofumi@mail.parknet.co.jp>
;; Version: 1.0.0
;; Created: 17 Nov 2025

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Search KeePassXC database.  The keywords are matching with
;; attributes in an entry.
;; 
;; `:user'             => "UserName" attribute
;; `:secret'           => "Password" attribute
;; `:host' and `:port' => "Host" additional attribute
;;
;; Like said in above, this uses "Host" to represent `:host' and
;; `:port' keywords.  The format of "Host" should be the following
;; format,
;;
;;    Host: <hostname1>, <hostname2>:<port1>, :<port2>, ...
;;
;; Config:
;;
;; `auth-sources' format is "kdbx:<KDBX PATH>".  If the element prefix
;; is "kdbx:", the element is treated as KeePassXC source.  And "<KDBX
;; PATH>" is the path to a database.
;;
;;   (add-to-list 'auth-sources "kdbx:~/.local/share/keepassxc/Passwords.kdbx")
;;
;; If the database requires a keyfile, you have to set
;; `auth-source-kpxc-keyfile'.

;;; Code:

(require 'auth-source)
(require 'secrets)
(require 'password-cache)
(eval-when-compile (require 'cl-lib))

(defgroup auth-source-kpxc nil
  "KeePassXC integration within auth-source."
  :group 'auth-source)

(defcustom auth-source-kpxc-keyfile nil
  "Alist of a database and a key file pair.

Each list element should the following form:

 (STRING . STRING)

The first string must be a path of database, the second string must be a
path of key file for the database.

For example,

  (setopt auth-source-kpxc-keyfile
          \\='((\"~/Passwords.kdbx\" . \"~/Passwords.bin\")))

With this, when open \"~/Passwords.kdbx\", \"~/Passwords.bin\" will be
used as keyfile."
  :type '(repeat (cons :tag "KeePassXC database/keyfile" file file)))

(defcustom auth-source-kpxc-match-url nil
  "`:host'/`:port' compares with \"URL\" field in an entry."
  :type 'boolean
  :version "31.0")

(defcustom auth-source-kpxc-command "keepassxc-cli"
  "The command to use to run keepassxc-cli."
  :type 'string)

(defun auth-source-kpxc-secrets-session-read (key)
  "Read passwd from cache for KEY."
  (secrets-get-secret "session" key))

(defun auth-source-kpxc-secrets-session-add (key passwd)
  "Add PASSWD to cache for KEY."
  (secrets-create-item "session" key passwd))

(defun auth-source-kpxc-secrets-session-del (key)
  "Delete cache for KEY."
  (secrets-delete-item "session" key))

(defvar auth-source-kpxc-cache-method-secrets-session
  (list
   :read #'auth-source-kpxc-secrets-session-read
   :add #'auth-source-kpxc-secrets-session-add
   :del #'auth-source-kpxc-secrets-session-del)
  "The password cache method by `secrets' session.")

(defvar auth-source-kpxc-cache-method-password-cache
  (list
   :read #'password-read-from-cache
   :add #'password-cache-add
   :del #'password-cache-remove)
  "The password cache method by `password-cache'.")

(defcustom auth-source-kpxc-cache-method
  (or (and secrets-enabled auth-source-kpxc-cache-method-secrets-session)
      auth-source-kpxc-cache-method-password-cache)
  "The method for password cache."
  :type 'plist)

(defun kpxc-read-password (file)
  "Read the password for FILE."
  (let ((password-cache-expiry auth-source-cache-expiry)
        (read-func (plist-get auth-source-kpxc-cache-method :read))
        (add-func (plist-get auth-source-kpxc-cache-method :add)))
    (when-let* ((key (expand-file-name file)))
      (if-let* ((passwd (funcall read-func key)))
          passwd
        (setq passwd (read-passwd (format "Enter Password (%s): " file)))
        (funcall add-func key passwd)
        passwd))))

(defun kpxc-delete-password (file)
  "Delete the password cache for FILE."
  (let ((password-cache-expiry auth-source-cache-expiry)
        (key (expand-file-name file))
        (del-func (plist-get auth-source-kpxc-cache-method :del)))
    (funcall del-func key)))

(defvar kpxc-process nil)

(defun kpxc-process-filter (proc string)
  "Receive the output of PROC as STRING."
  (when (buffer-live-p (process-buffer proc))
    ;; is this prompt?
    (process-put proc 'promptp (string-suffix-p "> " string))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (let ((inhibit-read-only t))
	    (insert string))
	  (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun kpxc-sentinel (proc _msg)
  "Run when the process PROC exits."
  (with-current-buffer (process-buffer proc)
    (let ((_code (process-exit-status proc)))
      (ignore-errors
        (kill-buffer (current-buffer))))))

(defun kpxc-get-response (proc)
  "Wait the prompt and return the output from PROC."
  (while (not (process-get proc 'promptp))
    (when (not (process-live-p proc))
      (user-error "Error: KeePassXC process exit unexpectedly" ))
    (accept-process-output nil 0.1))
  (process-put proc 'promptp nil)
  (with-current-buffer (process-buffer proc)
    (let ((start (progn
                   ;; skip a first line as the input command
                   (goto-char (point-min))
                   (forward-line 1)
                   (point)))
          (end (progn
                 ;; skip a last line as prompt
                 (goto-char (point-max))
                 (goto-char (line-beginning-position))
                 (point))))
      (prog1
          (buffer-substring start end)
        (erase-buffer)))))

(defmacro with-kpxc-process (file &rest body)
  "Execute the forms in BODY with KeePassXC client session for FILE."
  (declare (indent 1) (debug t))
  `(let* ((passwd (kpxc-read-password ,file))
          (keyfile (cdr (assoc ,file auth-source-kpxc-keyfile)))
          (file (expand-file-name ,file))
          (args (if keyfile
                    (list "-k" (expand-file-name keyfile) file)
                  (list file)))
          (process-connection-type nil)
          (kpxc-process
           (with-environment-variables (("LANG" "C.UTF-8"))
             (apply #'start-process
                    "KeePassXC" (generate-new-buffer " *kpxc process*")
                    auth-source-kpxc-command "open" args))))
     (set-process-query-on-exit-flag kpxc-process nil)
     (set-process-filter kpxc-process #'kpxc-process-filter)
     (set-process-sentinel kpxc-process #'kpxc-sentinel)

     ;; send password
     (condition-case err
         (progn
           (process-send-string kpxc-process (concat passwd "\n"))
           (kpxc-get-response kpxc-process))
       (error
        ;; remove cache if open was failed
        (kpxc-delete-password ,file)
        (signal 'user-error (cdr err))))

     (unwind-protect
         (with-current-buffer (process-buffer kpxc-process)
           ,@body)
       (when (process-live-p kpxc-process)
         (process-send-string kpxc-process "exit\n")))))

(defun kpxc-quote-string (str)
  "Quote \"STR\" for passing to KeePassXC interactive shell."
  (concat "\"" (string-replace "\"" "\\\"" str) "\""))

(defun kpxc-cmd-send (&rest args)
  "Send KeePassXC command with ARGS for FILE."
  (process-send-string kpxc-process (concat (string-join args " ") "\n"))
  (kpxc-get-response kpxc-process))

;; echo <PASS> | keepassxc-cli search <KDBX> -q 'url:<HOST> +u:<USER1>|<USER2>'
(defun kpxc-cmd-search (term)
  "Search TERM in FILE."
  (when-let* ((lines (kpxc-cmd-send "search" "-q" (kpxc-quote-string term))))
    ;; If success, `lines' should be the full path of entry
    (if (not (string-prefix-p "/" lines))
        nil
      (split-string lines "\n" t))))

;; echo <PASS> | keepassxc-cli show <KDBX> -q -s --all <ENTRY>
(defun kpxc-cmd-show (entry)
  "Get ENTRY in FILE."
  (when-let* ((lines (kpxc-cmd-send "show" "-q" "-s" "--all"
                                    (kpxc-quote-string entry))))
    (with-temp-buffer
      (insert lines)
      (goto-char (point-min))
      ;; parse output
      (save-excursion
        (let (alist label start notes-p)
          (while (not (eobp))
            ;; search a label
            (let* ((matchp (re-search-forward
                            "^\\(\\(KPH: \\)?\\(.*?\\)\\): " nil t))
                   (next-label (and matchp (match-string 1)))
                   (next-start (and matchp (point)))
                   (end (if matchp
                            (line-beginning-position)
                          (goto-char (point-max)))))

              ;; "Notes" is free style and not interesting much, so it
              ;; is ignored.  To skip, ignore lines until "Uuid" that
              ;; next of "Notes".
              (cond
               ((equal label "Notes")
                (setq notes-p t))
               ((equal label "Uuid")
                (setq notes-p nil)))

              (when (and label (not notes-p))
                (let ((value (string-trim-right
                              (buffer-substring start end) "[\n]")))
                  ;; normalize label/value
                  (cond
                   ((equal label "UserName")
                    (setq label :user))
                   ((equal label "Password")
                    (setq label :secret)
                    (setq value (let ((lexv (auth-source--obfuscate value)))
                                  (lambda ()
                                    (auth-source--deobfuscate lexv)))))
                   ((equal label "Host")
                    (setq label :host))
                   (t
                    (setq label (auth-source--symbol-keyword label))))

                  ;; check the duplicated label
                  (if (assoc label alist)
                      (warn "Found the duplicated label, ignored: \"%s\"" label)
                    (push (cons label value) alist))))

              (setq label next-label)
              (setq start next-start)))
          (nreverse alist))))))

(defun auth-source-kpxc-list (values)
  "Make sure VALUES to be a list."
  (if (and (atom values) (not (eq t values)))
      (list values)
    values))

(defun auth-source-kpxc-combination (hosts ports)
  "Make all combination of HOSTS and PORTS."
  (let ((hosts (auth-source-kpxc-list hosts))
        (ports (auth-source-kpxc-list ports)))
    (if (and (eq hosts t) (eq ports t))
        nil
      (let (result)
        (dolist (host (if (eq t hosts) '("") hosts))
          (dolist (port (if (eq t ports) '("") ports))
            (push (if (string-empty-p port)
                      host
                    (concat host ":" port))
                  result)))
        (nreverse result)))))

(defun auth-source-kpxc-build-term (key terms)
  "Build a search term from KEY and TERMS."
  (let ((terms (auth-source-kpxc-list terms)))
    (if (eq terms t)
        nil
      (concat key ":" (kpxc-quote-string (string-join terms "|"))))))

(defun auth-source-kpxc-convert (alist)
  "Convert ALIST to plist."
  (mapcar (lambda (entry)
            (let (result)
              (dolist (item entry result)
                (let ((k (car item))
                      (v (cdr item)))
                  (setq result (plist-put result k v))))))
          alist))

(defun auth-source-kpxc-split-by-host (alist)
  "Split \"Host\" field in ALIST.
\"Host\" can be the comma separated \"host:port\" list.  So this splits
\"Host\" and make a copied alist for each host."
  (let ((host-entry (or (cdr (assoc :host alist)) ""))
        pairs alists)
    ;; make (host . port) alist
    (dolist (entry (split-string host-entry "," t " *"))
      (when (string-match "\\([^:]*\\)\\(:\\(.*\\)\\)?" entry)
        (let* ((host (match-string 1 entry))
               (host (if (string-empty-p host) nil host))
               (port (match-string 3 entry))
               (port (if (string-empty-p port) nil port)))
          (cl-pushnew (cons host port) pairs :test #'equal))))

    (setq alist (assoc-delete-all :port (assoc-delete-all :host alist)))
    ;; copy alist for each host/port pairs
    (if (null pairs)
        (push alist alists)
      (dolist (pair (nreverse pairs))
        (let ((host (car pair))
              (port (cdr pair))
              (alist (copy-alist alist)))
          (when port
            (push (cons :port port) alist))
          (when host
            (push (cons :host host) alist))
          (push alist alists))))
    (nreverse alists)))

(cl-defun auth-source-kpxc-parse (&key file max host user port require
                                       &allow-other-keys)
  "Parse FILE and return a list of matching entries in the file.
HOST, USER and PORT to match.

Note that the MAX parameter is used so we can exit the parse early."
  (if (listp file)
      ;; We got already parsed contents; just return it.
      file
    (when (file-exists-p file)
      (setq port (auth-source-ensure-strings port))

      (let* ((host-port (auth-source-kpxc-combination host port))
             ;; host:port is fuzzy match to Host
             (host-term (auth-source-kpxc-build-term "_Host" host-port))
             ;; host:port is fuzzy match to URL
             (url-term (auth-source-kpxc-build-term "url" host-port))
             ;; "+user" is exact match to UserName
             (user-term (auth-source-kpxc-build-term "+user" user))
             (check (lambda (alist)
                      (and alist
                           (auth-source-search-collection
                            host
                            (auth-source--aget alist :host))
                           (auth-source-search-collection
                            port
                            (auth-source--aget alist :port))
                           (auth-source-search-collection
                            user
                            (auth-source--aget alist :user))
                           (or
                            ;; the required list of keys is nil, or
                            (null require)
                            ;; every element of require is in n (normalized)
                            (let ((n (nth 0 (auth-source-kpxc-convert
                                             (list alist)))))
                              (cl-loop for req in require
                                       always (plist-get n req)))))))
             entries)

        (with-kpxc-process file
          ;; search (if no terms, it will return all entries)
          (setq entries (kpxc-cmd-search (concat host-term " " user-term)))
          ;; additionally search URL that entry has no Host
          (when (and auth-source-kpxc-match-url url-term)
            (mapc (lambda (e) (add-to-list 'entries e))
                  (kpxc-cmd-search
                   (concat "!_Host:* " url-term " " user-term))))

          (let (result)
            (catch 'break
              (dolist (entry entries)
                (when-let* ((show-alist (kpxc-cmd-show entry)))
                  (dolist (alist (auth-source-kpxc-split-by-host show-alist))
                    ;; The search can be fuzzy match, so check if it is
                    ;; matching exactly, and also this checks for
                    ;; `require'.
                    (when (funcall check alist)
                      (push alist result)
                      (unless (< (length result) max)
                        (throw 'break nil)))))))
            (nreverse result)))))))

(cl-defun auth-source-kpxc-search (&rest spec
                                         &key backend require create delete
                                         type max host user port
                                         &allow-other-keys)
  "Given a property list SPEC, return search matches from the `:backend'.
See `auth-source-search' for details on SPEC."
  ;; just in case, check that the type is correct (null or same as the backend)
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid KeePassXC search: %s %s")

  ;; TODO
  (cl-assert (not delete) nil
             "The KeePassXC auth-source backend doesn't support deletion yet")

  (let* ((source (oref backend source))
         (result (when (file-exists-p source)
                   (auth-source-kpxc-convert
                    (auth-source-kpxc-parse
                     :max max
                     :require require
                     :file source
                     :host (or host t)
                     :user (or user t)
                     :port (or port t))))))

    (unless result
      ;; TODO
      (cl-assert
       (not create) nil
       "The KeePassXC auth-source backend doesn't support creation yet"))

    result))

;;;###autoload
(defun auth-source-backends-parser-kpxc (entry)
  "Decide if it uses a this backend for ENTRY."
  ;; take kdbx:XYZ and use it as KeePassXC file "XYZ" matching any
  ;; user, host, and protocol
  (when (and (stringp entry) (string-match "^kdbx:\\(.+\\)" entry))
    (setq entry `(:source (:kdbx ,(match-string 1 entry)))))

  (cond
   ((and
     (not (null (plist-get entry :source))) ; the source must not be nil
     (listp (plist-get entry :source))      ; and it must be a list
     (plist-get (plist-get entry :source) :kdbx))

    (let* ((source-spec (plist-get entry :source))
           (source (plist-get source-spec :kdbx)))

      (if (and auth-source-ignore-non-existing-file
               (not (file-exists-p source)))
          (when auth-source-debug
            (auth-source-do-warn
             "auth-source-backends-parser-kpxc: not existing file, ignoring spec: %S"
             entry))
        (auth-source-backend
         :source source
         :type 'kpxc
         :search-function #'auth-source-kpxc-search
         :create-function #'ignore))))))

;;;###autoload
(add-hook 'auth-source-backend-parser-functions
          #'auth-source-backends-parser-kpxc)

(provide 'auth-source-kpxc)
;;; auth-source-kpxc.el ends here
