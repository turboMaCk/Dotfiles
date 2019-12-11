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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" default)))
 '(fic-highlighted-words (quote ("FIXME" "TODO" "BUG" "HACK")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-new-repl))
 '(haskell-tags-on-save t)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   (quote
    (latex-preview-pane company-ghci flycheck-rtags company-rtags rtags cmake-ide cmake-mode presentation fic-mode reason-mode dockerfile-mode direnv nix-sandbox glsl-mode flycheck-haskell nix-mode nvm company-flow flow-minor-mode flycheck-flow rjsx-mode autumn-light-theme flymake-go gist htmlize jsx-mode go-mode go markdown-preview-mode helm-spotify twittering-mode rust-mode s scss-mode simple-httpd smartparens tide tuareg typescript-mode web-mode with-editor writegood-mode yaml-mode yasnippet wanderlust slack org-bullets org-pomodoro org-jira sublime-themes fontawesome cargo caml shm idris-mode bash-completion psc-ide psci purescript-mode helm-css-scss perspective which-key evil-org org-evil hindent robe multiple-cursors ob-restclient restclient nyan-mode)))
 '(purescript-mode-hook (quote (turn-on-purescript-indent)) t)
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(put 'downcase-region 'disabled nil)
