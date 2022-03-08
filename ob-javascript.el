;;; ob-javascript.el --- org-babel functions for javascript evaluation

;; Copyright (C) 2016 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-javascript
;; Keywords: org babel javascript
;; Version: 0.0.2
;; Created: 26th Nov 2016
;; Package-Requires: ((org "8"))

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
(defvar ob-javascript-process-output nil)

(defvar ob-javascript-eoe "\u2029")
(defvar ob-javascript-eoe-js "\\u2029")
(defvar ob-javascript-timeout 5)

(defgroup ob-javascript nil
  "org-babel functions for javascript evaluation"
  :group 'org)

(defcustom ob-javascript:browser-binary "chromium"
  "browser binary"
  :group 'ob-javascript
  :type 'string)

(defvar org-babel-default-header-args:javascript '())
(defvar ob-javascript-data-root (file-name-directory load-file-name))

(defcustom ob-javascript-babel-config-file
  (expand-file-name ".babelrc" ob-javascript-data-root)
  "Path to .babelrc file."
  :type 'file
  :group 'ob-javascript)

(defcustom ob-javascript-babel-node-modules-path "~/ob-javascript/node_modules/"
  "Directory with babel executable, plugins and presets."
  :type 'directory
  :group 'ob-javascript)

(defcustom ob-javascript-babel-options
  `("--config-file" ,ob-javascript-babel-config-file)
  "Options for compiling with babel.
Plugins and presets used in options should exists in `ob-javascript-babel-node-modules-path'."
  :type '(repeat string)
  :group 'ob-javascript)

(defun ob-javascript-js-var-to-js (var)
  "Convert VAR into a js variable.
Convert an elisp value into a string of js source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'ob-javascript-js-var-to-js var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun ob-javascript-variable-assignments:js (params)
  "Return list of Javascript statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "var %s=%s;"
                     (car pair) (ob-javascript-js-var-to-js (cdr pair))))
   (org-babel--get-vars params)))

(defvar ob-javascript-util-string (with-temp-buffer
                                    (insert-file-contents
                                     (expand-file-name "util.js" ob-javascript-data-root))
                                    (buffer-string)))

(defvar ob-javascript-repl-string
  (string-join (split-string (format "__ob_eval__ = %s\n
require('repl').start({
    prompt: '',
    input: process.stdin,
    output: process.stdout,
    ignoreUndefined: true,
    useColors: false,
    terminal: false
})" ob-javascript-util-string)) "\s"))

(defun ob-javascript-make-babel-command (&optional source-file target-file options)
  (string-join
   (delete nil (append
                `(,(and ob-javascript-babel-node-modules-path
                        (concat "NODE_PATH="
                                ob-javascript-babel-node-modules-path)))
                `(,(expand-file-name ".bin/babel" ob-javascript-babel-node-modules-path) ,source-file)
                `,(and target-file (list "--out-file" target-file))
                ob-javascript-babel-options  (list "&> /dev/null"))) "\s"))

(defun ob-javascript-trim-use-strict (body)
  (replace-regexp-in-string "['\"]use[\s\t]strict[\"'];?\n+" "" body))

(defun ob-javascript-babel-compile (body)
  (let ((temp-file (concat (temporary-file-directory)
                           (make-temp-name "script") ".tsx"))
        (temp-compiled-file (concat (temporary-file-directory)
                                    (make-temp-name "script") ".js"))
        (result)
        (command))
    (setq result (with-temp-file temp-file
                   (insert body)
                   (write-region nil nil temp-file)
                   (setq command
                         (ob-javascript-make-babel-command
                          temp-file temp-compiled-file))
                   (message command)
                   (with-output-to-temp-buffer (current-buffer)
                     (shell-command command))))
    (setq result (with-current-buffer
                     (delay-mode-hooks
                       (find-file-noselect temp-compiled-file)
                       (get-file-buffer temp-compiled-file))
                   (buffer-string)))
    (list 0 result)))

(defun org-babel-execute:javascript (body params)
  (setq body (org-babel-expand-body:generic
              body params (ob-javascript-variable-assignments:js params)))
  (let ((session (or (cdr (assoc :session params)) "default"))
        (result-type (cdr (assoc :result-type params)))
        (dir (cdr (assoc :dir params)))
        (file (cdr (assoc :file params)))
        (verbose (cdr (assoc :verbose params)))
        (compliled))
    (setq compliled (if (equal (cdr (assoc :babel params)) "yes")
                        (progn
                          (ob-javascript-ensure-project)
                          (ob-javascript-babel-compile body))
                      (list 't body)))
    (if-let ((compiled-body (when (car compliled)
                              (cadr compliled))))
        (cond ((string= "none" session)
               (ob-javascript--eval compiled-body file dir verbose))
              ((or
                (string-prefix-p "http://" session)
                (string-prefix-p "https://" session))
               (ob-javascript--ensure-browser-session session)
               (ob-javascript--get-result-value
                (ob-javascript--eval-in-browser-repl session compiled-body)))
              (t (ob-javascript--eval-with-session session compiled-body file)))
      (cadr body))))

(defun ob-javascript--output (result file)
  (unless file result))

(defun ob-javascript--eval (body file &optional dir verbose)
  (let ((tmp-source (org-babel-temp-file "javascript-"))
        (tmp (org-babel-temp-file "javascript-")))
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
    (ob-javascript--output
     (ob-javascript--shell-command-to-string
      (list (format "NODE_PATH=%s" (ob-javascript--node-path dir)))
      (list "node" tmp))
     file)))

(defun ob-javascript--eval-with-session (session body file)
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
  (with-temp-buffer
    (let ((process-environment (append '("NODE_NO_WARNINGS=1") environ process-environment)))
      (apply 'call-process (car command) nil t nil (cdr command))
      (buffer-string))))

(defun ob-javascript-resolve-module (dir &optional file)
  (when-let ((result (if file
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
  (ob-javascript-resolve-module dir "node_modules"))

(defun ob-javascript--node-path (&optional dir)
  (let ((result (replace-regexp-in-string
                 "[:]+" ":"
                 (concat
                  (string-join
                   (seq-uniq
                    (mapcar 'expand-file-name
                            (delete nil
                                    (append
                                     `(,ob-javascript-babel-node-modules-path
                                       ,(and dir (ob-javascript-resolve-node-modules-dir dir)))
                                     (when-let ((node-path (getenv "NODE_PATH")))
                                       (split-string node-path ":" t))))))
                   ":")
                  ":"))))
    result))

(defun ob-javascript--ensure-session (session)
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
                    "require('repl').start({  prompt: '', input: process.stdin, output: process.stdout, ignoreUndefined: true,useColors: false, terminal: false })"))
          (start-process name name "node" tmp)))
      (sit-for 0.5)
      (set-process-filter (get-process name)
                          'ob-javascript--process-filter))))

(defun ob-javascript--process-filter (process output)
  (setq ob-javascript-process-output
        (concat ob-javascript-process-output output)))

(defun ob-javascript--wait (timeout what)
  (while (and
          (or (null what)
              (null ob-javascript-process-output)
              (not (string-match-p what ob-javascript-process-output)))
          (> timeout 0))
    (setq timeout (- timeout 0.2))
    (sit-for 0.2)))

(defun ob-javascript--eval-in-repl (session body)
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
  (let ((name (format "*ob-javascript-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (let ((process (with-current-buffer
                         (get-buffer-create name)
                       (start-process
                        name name
                        ob-javascript:browser-binary
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
  (setq body (ob-javascript-trim-use-strict body))
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

(defun ob-javascript--get-result-value (response)
  (let* ((results (assoc-default 'result (json-read-from-string response)))
         (value (assoc-default 'value results))
         (description (assoc-default 'description results)))
    (or value description results response)))

(defun ob-javascript-exec-in-dir (command project-dir)
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
            (lambda (process state)
              (let ((output (with-current-buffer
                                (process-buffer process)
                              (buffer-string))))
                (kill-buffer (process-buffer process))
                (if (= (process-exit-status process) 0)
                    (progn
                      (message "finished"))
                  (user-error (format "%s\n%s" command output))))))
           (set-process-filter proc #'comint-output-filter))))

(defun ob-javascript-read-babel-config (&optional file)
  (with-temp-buffer
    (save-excursion (insert-file-contents file)
                    (let ((json-object-type 'alist)
                          (json-array-type 'list))
                      (json-read)))))

(defun ob-javascript-flatten (items)
  (mapcar (lambda (opt)
            (cond
             ((symbolp opt)
              opt)
             ((stringp opt)
              opt)
             ((null opt)
              opt)
             ((eq opt t)
              opt)
             ((consp opt)
              (if (listp (car opt))
                  (ob-javascript-flatten (car opt))
                (car opt)))
             ((and (listp opt))
              (mapcar 'ob-javascript-flatten opt))
             (t opt)))
          items))

(defun ob-javascript-get-config-dependencies ()
  (when-let ((configs
              (mapcar 'ob-javascript-read-babel-config
                      (seq-filter
                       'file-exists-p
                       (delete nil
                               `(,ob-javascript-babel-config-file))))))
    (let ((dependencies))
      (dolist (config configs)
        (let ((presets (ob-javascript-flatten (cdr (assoc 'presets config))))
              (plugins (ob-javascript-flatten (cdr (assoc 'plugins config))))
              (package-json))
          (setq dependencies (append dependencies presets plugins))))
      (append '("@babel/core" "@babel/cli") (seq-uniq (delete nil dependencies))))))

(defun ob-javascript-make-npm-install-command ()
  (when-let ((dependencies (ob-javascript-get-missing-dependencies)))
    (string-join (append '("npm install --save-dev") dependencies) "\s")))

(defun ob-javascript-get-missing-dependencies ()
  (seq-remove (lambda (it) (file-exists-p
                       (expand-file-name it ob-javascript-babel-node-modules-path)))
              (ob-javascript-get-config-dependencies)))

;;;###autoload
(defun ob-javascript-ensure-project ()
  (interactive)
  (when-let ((project-dir (replace-regexp-in-string
                           "/node_modules/?$" ""
                           ob-javascript-babel-node-modules-path)))
    (when (and (not (file-exists-p project-dir))
               (yes-or-no-p (format "Create directory %s?" project-dir)))
      (make-directory project-dir t))
    (when-let ((command (ob-javascript-make-npm-install-command)))
      (unless (file-exists-p (expand-file-name "package.json" project-dir))
        (setq command (concat "npm init -y && " command)))
      (when (yes-or-no-p (format "Run %s?" command))
        (ob-javascript-exec-in-dir command project-dir)))))

(provide 'ob-javascript)
;;; ob-javascript.el ends here
