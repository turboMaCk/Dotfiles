;;; Package --- Summary

;;; Commentary:
;; Main Emacs file responsible for loading all packages and configuration file.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.


;;; Code:
(package-initialize)
;(set-default-font "Menlo")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Custom loads
(add-to-list 'load-path
   (expand-file-name "plugin"
                     user-emacs-directory))

(require 'pallet)
(pallet-mode t)

(require 'org)
(require 'org-install)
(require 'ob-tangle)

;; Magit hack
;; see https://stackoverflow.com/questions/55986401/emacs-magit-requred-feature-isearch-was-not-provided
(provide 'isearch)

;; Load Config from Org file
(defun turbo_mack/load-config ()
  "Compile and load whole configuration."

  (org-babel-load-file
   (expand-file-name "turbo_mack.org"
                     user-emacs-directory)))

(turbo_mack/load-config)
(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
