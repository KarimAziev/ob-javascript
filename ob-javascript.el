;;; ob-javascript.el --- Org babel for javascript -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ob-javascript
;; Keywords: convenience, outlines
;; Version: 0.0.3
;; Created: 26th Nov 2016
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for javascript evaluation
;;

;;; Code:

(require 'ob)
(require 'json)
(require 'shell)

(defvar org-babel-tangle-lang-exts)

(defmacro ob-javascript--defvar (sym &rest body)
  "Define a namespaced variable with markdown suffix.

It is used to supress package lint warnings about a non-standard separator
and package prefix.

Argument SYM is the symbol to define as a variable.

Remaining arguments BODY are the forms that make up the body of the variable
definition."
  (declare (doc-string 3)
           (indent 2))
  `(defvar ,(intern (concat (symbol-name sym) ":javascript"))
     ,@body))

(add-to-list 'org-babel-tangle-lang-exts '("javascript" . "js"))

(defvar ob-javascript-process-output nil)
(defvar ob-javascript-eoe "\u2029")
(defvar ob-javascript-eoe-js "\\u2029")
(defvar ob-javascript-timeout 5)


(defgroup ob-javascript nil
  "Org-babel functions for javascript evaluation."
  :group 'org-babel)

(defcustom ob-javascript-browser-binary "chromium"
  "Browser binary."
  :group 'ob-javascript
  :type 'string)


(ob-javascript--defvar org-babel-header-args '((babel . ((yes no))))
  "Javascript specific header args.")

(defvar org-babel-default-header-args:javascript '((:babel . "yes")))
(defvar ob-javascript-data-root (file-name-directory load-file-name))

(defcustom ob-javascript-babel-config-file
  (expand-file-name ".babelrc" ob-javascript-data-root)
  "Path to .babelrc file."
  :type 'file
  :group 'ob-javascript)

(defcustom ob-javascript-babel-node-modules-path (expand-file-name
                                                  "node_modules"
                                                  ob-javascript-data-root)
  "Directory with babel executable, plugins and presets."
  :type 'directory
  :group 'ob-javascript)

(defcustom ob-javascript-babel-options
  `("--config-file" ,ob-javascript-babel-config-file)
  "Options for compiling with babel.
Plugins and presets used in options should exists in
`ob-javascript-babel-node-modules-path'."
  :type '(repeat string)
  :group 'ob-javascript)

(defun ob-javascript-js-var-to-js (var)
  "Convert VAR into a js variable.
Convert an elisp value into a string of js source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'ob-javascript-js-var-to-js var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-variable-assignments:javascript (params)
  "Return the babel variable assignments in PARAMS."
  (mapcar
   (lambda (pair) (format "var %s=%s;"
                     (car pair) (ob-javascript-js-var-to-js (cdr pair))))
   (org-babel--get-vars params)))

(defvar ob-javascript-util-string
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "util.js" ob-javascript-data-root))
    (buffer-string)))

(defvar ob-javascript-repl-string
  "require('repl').start({  prompt: '', input: process.stdin, output: process.stdout, ignoreUndefined: true, useColors: false, terminal: false })")

(defun ob-javascript-make-babel-command (&optional source-file target-file)
  "Return shell command string for compiling SOURCE-FILE into TARGET-FILE."
  (string-join
   (delete nil (append
                `(,(and ob-javascript-babel-node-modules-path
                    (concat "NODE_PATH="
                     ob-javascript-babel-node-modules-path)))
                `(,(expand-file-name ".bin/babel"
                    ob-javascript-babel-node-modules-path)
                  ,source-file)
                `,(and target-file (list "--out-file" target-file))
                ob-javascript-babel-options))
   "\s"))

(defun ob-javascript-normalize-body-for-chrome-repl (body)
  "Remove use-strict and new lines from BODY."
  (string-join (split-string (replace-regexp-in-string
                              "['\"]use[\s\t]strict[\"'];?\n+" ""
                              body)
                             nil t)
               "\s"))

(defun ob-javascript-babel-compile (body)
  "Compile BODY and return list with status code and result."
  (let ((temp-file (concat (temporary-file-directory)
                           (make-temp-name "script") ".tsx"))
        (temp-compiled-file (concat (temporary-file-directory)
                                    (make-temp-name "script") ".js"))
        (result)
        (command))
    (with-temp-file temp-file
      (insert body)
      (write-region nil nil temp-file)
      (setq command
            (ob-javascript-make-babel-command
             temp-file temp-compiled-file))
      (message command)
      (with-output-to-temp-buffer (current-buffer)
        (shell-command command)))
    (setq result (with-temp-buffer
                   (insert-file-contents temp-compiled-file nil)
                   (buffer-string)))
    (list 0 result)))

(defun ob-javascript-read-results (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results)
            (string-prefix-p "[" results)
            (string-match-p "\\][\s\t\n]*;?$" results))
       (let ((res (string-trim results)))
         (org-babel-read
          (concat "'"
                  (replace-regexp-in-string
                   "\\[" "(" (replace-regexp-in-string
                              "\\]" ")" (replace-regexp-in-string
                                         ",[[:space:]]" " "
                                         (replace-regexp-in-string
                                          "'" "\"" res)))))))
     results)))

;;;###autoload
(defun org-babel-execute:javascript (body params)
  "Execute a block of Javascript code BODY with org-babel.
This function is called by `org-babel-execute-src-block'.
PARAMS is alist."
  (setq body (org-babel-expand-body:generic
              body params (org-babel-variable-assignments:javascript params)))
  (let ((session (or (cdr (assoc :session params)) "default"))
        (file (cdr (assoc :file params)))
        (dir (cdr (assoc :dir params)))
        (verbose (cdr (assoc :verbose params)))
        (compliled))
    (setq compliled (if (equal (cdr (assoc :babel params)) "yes")
                        (progn
                          (ob-javascript-ensure-project)
                          (ob-javascript-babel-compile body))
                      (list 't body)))
    (if-let* ((compiled-body
              (when (car compliled)
                (cadr compliled))))
        (let ((result
               (cond ((string= "none" session)
                      (ob-javascript--eval compiled-body
                                           file
                                           dir
                                           verbose))
                     ((or
                       (string-prefix-p "http://" session)
                       (string-prefix-p "https://" session))
                      (ob-javascript--ensure-browser-session
                       session)
                      (ob-javascript--get-result-value
                       (ob-javascript--eval-in-browser-repl
                        session compiled-body)))
                     (t (ob-javascript--eval-with-session
                         session compiled-body file)))))
          (org-babel-result-cond (cdr (assq :result-params params))
            result (ob-javascript-read-results result)))
      (cadr body))))

(defun ob-javascript--output (result file)
  "Return RESULT when FILE is nil."
  (unless file result))

(defun ob-javascript--eval (body file &optional directory verbose)
  "Eval BODY in the context of DIRECTORY.
If VERBOSE and FILE is non nil, output will be expanded."
  (let ((tmp-source (org-babel-temp-file "javascript-"))
        (tmp (org-babel-temp-file "javascript-"))
        (result))
    (with-temp-file tmp-source
      (insert body))
    (with-temp-file tmp
      (insert (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "util.js" ob-javascript-data-root))
                (buffer-string)))
      (insert (format (if verbose
                          "__ob_eval__('%s', '', '%s', true)"
                        "__ob_eval__('%s', '', '%s')")
                      tmp-source
                      (or file ""))))
    (setq result
          (ob-javascript--output
           (ob-javascript--shell-command-to-string
            (list (format "NODE_PATH=%s" (ob-javascript--node-path directory)))
            (list "node" tmp))
           file))
    (dolist (temp-file (list tmp-source tmp))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))
    result))

(defun ob-javascript--eval-with-session (session body file)
  "Evaluate BODY in SESSION and return result if FILE is nil."
  (let ((tmp (org-babel-temp-file "javascript-")))
    (ob-javascript--ensure-session session)
    (with-temp-file tmp (insert body))
    (ob-javascript--output
     (ob-javascript--eval-in-repl
      session
      (format
       "__ob_eval__('%s', '%s', '%s')" tmp ob-javascript-eoe-js (or file "")))
     file)))

(defun ob-javascript--shell-command-to-string (environ command)
  "Run shell COMMAND in current `process-environment' merged with ENVIRON."
  (with-temp-buffer
    (let ((process-environment
           (append
            '("NODE_NO_WARNINGS=1") environ process-environment)))
      (apply #'call-process (car command) nil t nil (cdr command))
      (buffer-string))))

(defun ob-javascript-resolve-module (dir &optional file)
  "Starting at DIR look up directory hierarchy for FILE.
If found return full path to FILE."
  (when-let* ((result (if file
                         (let ((default-directory (expand-file-name dir))
                               (found))
                           (setq found (locate-dominating-file
                                        default-directory file))
                           (when found
                             (expand-file-name file found)))
                       (and (file-exists-p dir)
                            dir))))
    (if (and (file-directory-p result)
             (not (string-match-p result "/$")))
        (concat result "/")
      result)))

(defun ob-javascript-resolve-node-modules-dir (dir)
  "Starting at DIR look up directory hierarchy for node_modules.
If found return path to node_modules."
  (ob-javascript-resolve-module dir "node_modules"))

(defun ob-javascript-node-modules-global ()
  "Return directory with global npm dependencies."
  (when-let* ((dir (string-trim
                   (shell-command-to-string
                    "npm config get prefix"))))
    (setq dir (expand-file-name "lib/node_modules/" dir))
    (when (file-exists-p dir)
      dir)))

(defun ob-javascript--node-path (&optional dir)
  "Add DIR to node path."
  (let ((result (replace-regexp-in-string
                 "[:]+" ":"
                 (concat
                  (string-join
                   (seq-uniq
                    (mapcar
                     #'expand-file-name
                     (delete
                      nil
                      (append
                       `(,ob-javascript-babel-node-modules-path
                         ,(and dir (ob-javascript-resolve-node-modules-dir
                                    dir))
                         ,(ob-javascript-node-modules-global))
                       (when-let* ((node-path (getenv "NODE_PATH")))
                         (split-string node-path ":" t))))))
                   ":")
                  ":"))))
    result))

(defun ob-javascript--ensure-session (session)
  "Ensure SESSION."
  (let ((name (format "*javascript-%s*" session))
        (node-path (ob-javascript--node-path))
        (tmp (concat (org-babel-temp-file "repl") ".js")))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (let ((process-environment
               (append (list "NODE_NO_READLINE=1"
                             (format "NODE_PATH=%s" node-path))
                       process-environment)))
          (with-temp-file tmp
            (insert "__ob_eval__ ="
                    ob-javascript-util-string
                    "\n"
                    ob-javascript-repl-string))
          (start-process name name "node" tmp)))
      (sit-for 0.5)
      (set-process-filter (get-process name)
                          'ob-javascript--process-filter))))

(defun ob-javascript--process-filter (_process output)
  "Concat `ob-javascript-process-output' with OUTPUT."
  (setq ob-javascript-process-output
        (concat ob-javascript-process-output output)))

(defun ob-javascript--wait (timeout what)
  "Redisplay, then wait for TIMEOUT depending on WHAT."
  (while (and
          (or (null what)
              (null ob-javascript-process-output)
              (not (string-match-p what ob-javascript-process-output)))
          (> timeout 0))
    (setq timeout (- timeout 0.2))
    (sit-for 0.2)))

(defun ob-javascript--eval-in-repl (session body)
  "Eval BODY in SESSION repl."
  (let ((name (format "*javascript-%s*" session)))
    (setq ob-javascript-process-output nil)
    (process-send-string name (format "%s\n" body))
    (accept-process-output (get-process name) nil nil 1)
    (ob-javascript--wait ob-javascript-timeout ob-javascript-eoe)
    (message
     (replace-regexp-in-string
      ob-javascript-eoe
      "" ob-javascript-process-output))))

(defun ob-javascript--ensure-browser-session (session)
  "Create chromium browser SESSION."
  (let ((name (format "*ob-javascript-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (let ((process (with-current-buffer
                         (get-buffer-create name)
                       (start-process
                        name name
                        (or
                         (executable-find "chromium")
                         (executable-find "google-chrome"))
                        "--headless"
                        "--disable-gpu"
                        "--repl" session))))
        (sit-for 1)
        (set-process-filter
         process
         'ob-javascript--process-filter)
        (ob-javascript--wait
         ob-javascript-timeout
         "Type a Javascript expression to evaluate or \"quit\" to exit.")))))

(defun ob-javascript--eval-in-browser-repl (session body)
  "Evaluate BODY in browser repl SESSION."
  (setq body (ob-javascript-normalize-body-for-chrome-repl body))
  (let ((name (format "*ob-javascript-%s*" session)))
    (setq ob-javascript-process-output "")
    (process-send-string name (format "%s\n\"%s\"\n" body ob-javascript-eoe))
    (accept-process-output (get-process name) nil nil 1)
    (ob-javascript--wait ob-javascript-timeout ob-javascript-eoe)
    (replace-regexp-in-string
     "^>>> " ""
     (replace-regexp-in-string
      (format "^.*\"%s\".*$" ob-javascript-eoe)
      "" ob-javascript-process-output))))

(defun ob-javascript--get-result-value (result)
  "Extract and return the `value' or `description' from a JSON string RESULT.

Argument RESULT is a JSON string representing the result to parse."
  (let* ((result (assoc-default 'result (json-read-from-string result)))
         (value (assoc 'value result)))
    (if value (cdr value)
      (assoc-default 'description result))))

(defun ob-javascript-exec-in-dir (command project-dir)
  "Execute COMMAND in PROJECT-DIR."
  (let ((proc)
        (buffer (generate-new-buffer (format "*%s*" command)))
        (command command))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (if (file-exists-p project-dir)
                 (setq default-directory project-dir)
               (mkdir project-dir t)
               (setq default-directory project-dir))
             (setq proc (start-process-shell-command
                         (nth 0
                              (split-string command nil t))
                         buffer command))
             (shell-command-save-pos-or-erase)
             (shell-mode)
             (view-mode +1))
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let ((output (with-current-buffer
                                (process-buffer process)
                              (buffer-string))))
                (kill-buffer (process-buffer process))
                (if (= (process-exit-status process) 0)
                    (progn
                      (message "finished"))
                  (user-error (format "%s\n%s" command output))))))
           (set-process-filter proc #'comint-output-filter))))

(defun ob-javascript-read-babel-config (file)
  "Read json FILE and return alist."
  (with-temp-buffer
    (save-excursion (insert-file-contents file)
                    (let ((json-object-type 'alist)
                          (json-array-type 'list))
                      (json-read)))))

(defun ob-javascript-flatten-alist (alist)
  "Flattenize ALIST."
  (mapcar (lambda (opt)
            (cond
             ((seq-find (lambda (f) (funcall f opt))
                        '(booleanp symbolp stringp null))
              opt)
             ((consp opt)
              (if (listp (car opt))
                  (ob-javascript-flatten-alist (car opt))
                (car opt)))
             ((and (listp opt))
              (mapcar #'ob-javascript-flatten-alist opt))
             (t opt)))
          alist))

(defun ob-javascript-get-config-dependencies ()
  "Return babel dependencies based on `ob-javascript-babel-config-file'."
  (when-let* ((configs
              (mapcar #'ob-javascript-read-babel-config
                      (seq-filter
                       #'file-exists-p
                       (delete nil
                               `(,ob-javascript-babel-config-file))))))
    (let ((dependencies))
      (dolist (config configs)
        (let ((presets (ob-javascript-flatten-alist (cdr
                                                     (assoc 'presets config))))
              (plugins (ob-javascript-flatten-alist (cdr
                                                     (assoc 'plugins config)))))
          (setq dependencies (append dependencies presets plugins))))
      (append '("@babel/core" "@babel/cli") (seq-uniq
                                             (delete nil
                                                     dependencies))))))

(defun ob-javascript-make-npm-install-command ()
  "Return npm install command string with missed dependencies for babel."
  (when-let* ((dependencies (ob-javascript-get-missing-dependencies)))
    (string-join (append '("npm install --save-dev") dependencies) "\s")))

(defun ob-javascript-get-missing-dependencies ()
  "Return missing dependencies for babel."
  (seq-remove (lambda (it) (file-exists-p
                       (expand-file-name
                        it
                        ob-javascript-babel-node-modules-path)))
              (ob-javascript-get-config-dependencies)))

;;;###autoload
(defun ob-javascript-ensure-project ()
  "Interactivelly create directory with babel and plugins."
  (interactive)
  (let ((project-dir (file-name-parent-directory
                      ob-javascript-babel-node-modules-path)))
    (when (and (not (file-exists-p ob-javascript-babel-node-modules-path))
               (yes-or-no-p (format "Create directory %s?"
                                    ob-javascript-babel-node-modules-path)))
      (make-directory project-dir t))
    (when-let* ((command (ob-javascript-make-npm-install-command)))
      (unless (file-exists-p (expand-file-name "package.json" project-dir))
        (setq command (concat "npm init -y && " command)))
      (when (yes-or-no-p (format "Run %s?" command))
        (ob-javascript-exec-in-dir command project-dir)))))

(provide 'ob-javascript)
;;; ob-javascript.el ends here
