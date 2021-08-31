;;; ob-javascript.el --- org-babel functions for javascript evaluation

;; Copyright (C) 2016 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-javascript
;; Keywords: org babel javascript
;; Version: 0.0.1
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

(defcustom ob-javascript-babel-options
  '("--presets=@babel/preset-env,@babel/preset-typescript,@babel/preset-react"
    "--plugins @babel/plugin-transform-runtime")
  "Options for npx babel."
  :type '(repeat string)
  :group 'ob-javascript)

(defcustom ob-javascript-babel-node-modules-path nil
  "NODE_PATH for npx babel."
  :type 'directory
  :group 'ob-javascript)

(defvar ob-javascript-eval-function-string
  "function(path, eoe, outputPath) {
    result = eval(require('fs').readFileSync(path, {encoding:'utf8'}))
    write = function(obj) {
        if (outputPath) {
            if (obj instanceof Buffer) {
                require('fs').writeFile(outputPath, obj)
            } else if (obj && 'function' === typeof obj.pipe) {
                obj.pipe(require('fs').createWriteStream(outputPath))
            }
        } else {
            console.log(obj)
        }
        process.stdout.write(eoe)
    }
    if (result && 'function' === typeof result.then) {
        result.then(write, write)
    } else {
        write(result)
    }
}\n")

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

(defvar ob-javascript-util-string
  (concat "
__ob_eval__ = " ob-javascript-eval-function-string))

(defvar ob-javascript-repl-string
  (format "__ob_eval__ = %s\n
require('repl').start({
    prompt: '',
    input: process.stdin,
    output: process.stdout,
    ignoreUndefined: true,
    useColors: false,
    terminal: false
})
" ob-javascript-eval-function-string))

(defcustom ob-javascript-path-to-lib default-directory
  "Path to `ob-javascript'"
  :type 'directory)

(defun ob-javascript-make-babel-command (source-file target-file)
  (string-join
   (delete nil (append
                `(,(and ob-javascript-babel-node-modules-path
                        (concat "NODE_PATH="
                                ob-javascript-babel-node-modules-path)))
                `("npx babel" ,source-file)
                `("--out-file" ,target-file)
                ob-javascript-babel-options)) "\s"))

(defun ob-javascript-trim-use-strict (body)
  (replace-regexp-in-string "['\"]use[\s\t]strict[\"'];?\n+" "" body))

(defun ob-javascript-compile (body)
  (let ((temp-file (concat (temporary-file-directory)
                           (make-temp-name "script") ".ts"))
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
      (shell-command command))
    (setq result (with-current-buffer
                     (delay-mode-hooks
                       (find-file-noselect temp-compiled-file)
                       (get-file-buffer temp-compiled-file))
                   (buffer-string)))
    result))

(defun org-babel-execute:javascript (body params)
  (setq body (org-babel-expand-body:generic
              body params (ob-javascript-variable-assignments:js params)))
  (let ((session (or (cdr (assoc :session params)) "default"))
        (result-type (cdr (assoc :result-type params)))
        (dir (cdr (assoc :dir params)))
        (file (cdr (assoc :file params)))
        (body (if (assoc :babel params)
                  (ob-javascript--babel body)
                (list 't body))))
    (if (car body)
        (if (string= "none" session)
            (ob-javascript--eval (cadr body) file dir)
          (if (or
               (string-prefix-p "http://" session)
               (string-prefix-p "https://" session))
              (progn
                (ob-javascript--ensure-browser-session session)
                (ob-javascript--get-result-value
                 (ob-javascript--eval-in-browser-repl session (cadr body))))
            (ob-javascript--eval-with-session session (cadr body) file)))
      (cadr body))))

(defun ob-javascript--output (result file)
  (unless file result))

(defun ob-javascript--babel (source)
  (with-temp-buffer
    (insert source)
    (list
     (eq 0
         (call-process-region (point-min) (point-max) "babel" t t nil))
     (buffer-string))))

(defun ob-javascript--eval (body file &optional dir)
  (setq body (ob-javascript-trim-use-strict (ob-javascript-compile
                                             body)))
  (let ((tmp-source (org-babel-temp-file "javascript-"))
        (tmp (org-babel-temp-file "javascript-")))
    (with-temp-file tmp-source
      (insert body))
    (with-temp-file tmp
      (insert ob-javascript-util-string)
      (insert (format "__ob_eval__('%s', '', '%s')" tmp-source (or file ""))))
    (ob-javascript--output
     (ob-javascript--shell-command-to-string
      (list (format "NODE_PATH=%s" (ob-javascript--node-path dir)))
      (list "node" tmp))
     file)))

(defun ob-javascript--eval-buffer ()
  (interactive)
  (let ((body (buffer-substring-no-properties (point-min) (point-max))))
    (print (ob-javascript--eval body nil))))

(defun ob-javascript--eval-with-session (session body file)
  (setq body (ob-javascript-compile body))
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
    (let ((process-environment (append environ process-environment)))
      (apply 'call-process (car command) nil t nil (cdr command))
      (buffer-string))))

(defun ob-javascript-resolve-dir (dir)
  (let ((default-directory (expand-file-name dir))
        (found))
    (setq found (locate-dominating-file
                 default-directory "node_modules"))
    (when found
      (expand-file-name "node_modules" found))))

(defun ob-javascript--node-path (&optional dir)
  (let ((result (replace-regexp-in-string
                 "[:]+" ":"
                 (concat
                  (string-join
                   (seq-uniq
                    (delete nil
                            (append
                             `(,ob-javascript-babel-node-modules-path
                               ,(and dir (ob-javascript-resolve-dir dir))
                               ,(getenv "NODE_PATH")))))
                   ":")
                  ":"))))
    result))

(defun ob-javascript--ensure-session (session)
  (let ((name (format "*javascript-%s*" session))
        (node-path (ob-javascript--node-path))
        (tmp (org-babel-temp-file "repl")))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (let ((process-environment
               (append (list "NODE_NO_READLINE=1"
                             (format "NODE_PATH=%s" node-path))
                       process-environment)))
          (start-process (concat "node -e " "'" ob-javascript-repl-string "'"))
          ;; (with-temp-file tmp
          ;;   (insert ob-javascript-repl-string)
          ;;   (start-process name name "node -e" tmp))
          ))
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
  (setq body (ob-javascript-trim-use-strict (ob-javascript-compile body)))
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
  (setq body (ob-javascript-trim-use-strict (ob-javascript-compile body)))
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
  (let* ((result (assoc-default 'result (json-read-from-string result)))
         (value (assoc-default 'value result)))
    (or value result)))

(provide 'ob-javascript)
;;; ob-javascript.el ends here
