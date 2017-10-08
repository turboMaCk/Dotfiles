;;; ob-elm.el --- org-babel functions for elm evaluation

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-elm
;; Keywords: org babel elm
;; Version: 0.0.1
;; Created: 15th Oct 2015
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; org-babel functions for elm evaluation
;;

;;; Code:
(require 'ob)

(defvar ob-elm-process-output nil)

(defvar ob-elm-eoe "ob-elm-eoe")

(defun org-babel-execute:elm (body params)
  (let ((session (cdr (assoc :session params))))
    (ob-elm-ensure-session session params)
    (ob-elm-eval session body)))

(defun ob-elm-eval (session body)
  (let ((result (ob-elm-eval-in-repl session body)))
    (replace-regexp-in-string
     "\n\\(> \\)+" "" result)))

(defun ob-elm-ensure-session (session params)
  (let ((name (format "*elm-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (start-process name name "elm-repl"))
      (sit-for 0.5)
      (set-process-filter (get-process name) 'ob-elm-process-filter))))

(defun ob-elm-process-filter (process output)
  (setq ob-elm-process-output (concat ob-elm-process-output output)))

(defun ob-elm-wait ()
  (while (not (string-match-p (format "\"%s\" : String" ob-elm-eoe) ob-elm-process-output))
    (sit-for 0.2)))

(defun ob-elm-eval-in-repl (session body)
  (let ((name (format "*elm-%s*" session)))
    (setq ob-elm-process-output nil)
    (process-send-string name (format "%s\n" body))
    (accept-process-output (get-process name) nil nil 1)
    (sit-for 1)  ;; FIXME
    (process-send-string name (format "\"%s\"\n" ob-elm-eoe))
    (ob-elm-wait)
    (message ob-elm-process-output)
    (replace-regexp-in-string
     (regexp-quote (format "\"%s\" : String" ob-elm-eoe)) ""
     ob-elm-process-output)))

(provide 'ob-elm)
;;; ob-elm.el ends here
