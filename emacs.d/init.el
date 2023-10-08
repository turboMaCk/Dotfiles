;;; Package --- Summary

;;; Commentary:
;; Main Emacs file responsible for loading all packages and configuration file.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.


;;; Code:
;; Custom loads
(add-to-list 'load-path
   (expand-file-name "plugin"
                     user-emacs-directory))

;; Disable package.el
;; we're going to use straight.el instead
(setq package-enable-at-startup nil)

;; Load straight.el
;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

;; Magit hack
;; see https://stackoverflow.com/questions/55986401/emacs-magit-requred-feature-isearch-was-not-provided
(provide 'isearch)


;; Dependecies
(straight-use-package 'helm)
(straight-use-package 'dap-mode)
(straight-use-package 'simple-httpd)
(straight-use-package 'autumn-light-theme)
(straight-use-package 'bash-completion)
(straight-use-package 'caml)
(straight-use-package 'js2-mode)
(straight-use-package 'npm-mode)
(straight-use-package 'cargo)
(straight-use-package 'centered-window)
(straight-use-package 'cmake-ide)
(straight-use-package 'cmake-mode)
(straight-use-package 'color-identifiers-mode)
(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'company-ghci)
(straight-use-package 'company-rtags)
(straight-use-package 'dhall-mode)
(straight-use-package 'diminish)
(straight-use-package 'direnv)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'editorconfig)
(straight-use-package 'elm-mode)
(straight-use-package 'ess)
(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-org)
(straight-use-package 'evil-smartparens)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-visualstar)
(straight-use-package 'fic-mode)
(straight-use-package 'fontawesome)
(straight-use-package 'gdscript-mode)
(straight-use-package 'glsl-mode)
(straight-use-package 'go)
(straight-use-package 'go-mode)
(straight-use-package 'graphene)
(straight-use-package 'handlebars-mode)
(straight-use-package 'haskell-mode)
(straight-use-package 'helm)
(straight-use-package 'helm-ag)
(straight-use-package 'helm-css-scss)
(straight-use-package 'helm-projectile)
(straight-use-package 'hindent)
(straight-use-package 'htmlize)
(straight-use-package 'idris-mode)
(straight-use-package 'ini-mode)
(straight-use-package 'key-chord)
(straight-use-package 'latex-preview-pane)
(straight-use-package 'litex-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'magit)
(straight-use-package 'markdown-preview-mode)
(straight-use-package 'multiple-cursors)
(straight-use-package 'nix-mode)
(straight-use-package 'nix-sandbox)
(straight-use-package 'nodejs-repl)
(straight-use-package 'npm-mode)
(straight-use-package 'nyan-mode)
(straight-use-package 'ob-restclient)
(straight-use-package 'ob-typescript)
(straight-use-package 'org)
(straight-use-package 'org-bullets)
(straight-use-package 'org-evil)
(straight-use-package 'org-ref)
(straight-use-package 'ormolu)
(straight-use-package 'pbcopy)
(straight-use-package 'perspective)
(straight-use-package 'php-mode)
(straight-use-package 'presentation)
(straight-use-package 'projectile)
(straight-use-package 'psc-ide)
(straight-use-package 'psci)
(straight-use-package 'purescript-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'reason-mode)
(straight-use-package 'recentf)
(straight-use-package 'restclient)
(straight-use-package 'robe)
(straight-use-package 'rtags)
(straight-use-package 'rust-mode)
(straight-use-package 'rustic)
(straight-use-package 's)
(straight-use-package 'saveplace)
(straight-use-package 'scss-mode)
(straight-use-package 'shm)
(straight-use-package 'smartparens)
(straight-use-package 'smooth-scrolling)
(straight-use-package 'svelte-mode)
(straight-use-package 'terraform-mode)
(straight-use-package 'tide)
(straight-use-package 'toml-mode)
(straight-use-package 'tuareg)
(straight-use-package 'typescript-mode)
(straight-use-package 'undo-fu)
(straight-use-package 'web-mode)
(straight-use-package 'which-key)
(straight-use-package 'with-editor)
(straight-use-package 'writegood-mode)
(straight-use-package 'xclip)
(straight-use-package 'yaml-mode)
(straight-use-package 'yasnippet-snippets)
(if (not (eq system-type 'windows-nt))
  (straight-use-package 'exec-path-from-shell)
  (print "Skipping exec-path-from-shell on Windows")
)


(setq user-full-name "Marek Fajkus"
  user-mail-address "marek.faj@gmail.com")

(setq custom-file (make-temp-file "emacs-custom"))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (if (display-graphic-p)
    (if (memq window-system '(mac ns))
      (menu-bar-mode t)
      (menu-bar-mode -1))
    (menu-bar-mode -1)))


(setq initial-major-mode 'text-mode)

(add-hook 'conf-toml-mode-hook 'prog-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

(when (display-graphic-p)
  (fringe-mode 2)
  (setq-default left-fringe-width 12)
  (setq-default right-fringe-width 0))

(setq-default truncate-lines t)
(setq line-spacing 3)
(set-face-attribute 'default nil :height 100)

(custom-set-variables '(x-select-enable-clipboard t))

(setq-default compilation-always-kill t)
(setq compilation-ask-about-save nil)

(add-hook 'compilation-filter-hook
  (lambda ()
    (when (eq major-mode 'compilation-mode)
      (require 'ansi-color)
      (let ((inhibit-read-only t))
        (ansi-color-applu-on-region (point-min) (point-max))))))

;; Bookamrks
(setq-default bookmark-default-file "~/.emacs.d/bookmakrs")
(setq-default bookmark-save-flag t)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; History
(setq-default savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq history-length t)
(setq history-delete-duplicates t)
(setq-default savehist-save-minibuffer-history t)
(setq-default savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring))

;; Edif
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; re-builder
(setq-default reb-re-syntax 'string) ;; fix backslash madness

(when (fboundp 'winner-mode)
  (winner-mode t))

(require 'midnight)
(midnight-delay-set 'midnight-delay 0)

;; saveplace
(require 'saveplace)
(setq-default save-place t)

;; smooth-scrolling
(require 'smooth-scrolling)

;; recentf
(require 'recentf)
(recentf-mode t)
(setq recentf-save-file "~/.emacs.d/recentf")
(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 500)
(setq recentf-auto-cleanup 300)
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(run-with-timer 1800 1800 'recentf-save-list)

;; pcomplete
(setq-default pcomplete-ignore-case t)

;; imenu
(setq-default imenu-auto-rescan t)

;; narrowing
(put 'narrow-to-region 'disabled nil)

;; dired
(require 'dired)
(add-hook 'dired-load-hook
  (function (lambda () (load "dired-x"))))

;; create files from dired mode
(define-key dired-mode-map "c" 'find-file)

;; ibuffer
(setq-default ibuffer-expert t)
(setq-default ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

;; hl line mode
(global-hl-line-mode t)

(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 3)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)

(defun turbo_mack/do-not-kill-scratch-buffer ()
    "PREVENT KILLING SCRATCH BUFFERS!"

    (if (member (buffer-name (current-buffer))
                '("*scratch*" "*Messages*" "*Require Times*"))
      (progn
        (bury-buffer)
        nil)
    t))

(add-hook 'kill-buffer-query-functions 'turbo_mack/do-not-kill-scratch-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)
(setq save-interprogram-paste-before-kill t)
(setq create-lockfiles nil)
(setq echo-keystrokes 0.01)
(setq initial-major-mode 'emacs-lisp-mode)
(setq eval-expression-print-level nil)
(setq-default indent-tabs-mode nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(xterm-mouse-mode t)
(which-function-mode t)
(blink-cursor-mode -1)
(global-auto-revert-mode t)
(electric-indent-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(random t) ;; seed

;; disable anoying gui popups
(setq use-dialog-box nil)

(if (eq system-type 'windows-nt)
    ;; use %USERPROFILE% on Windows
    (setq default-directory (concat (getenv "USERPROFILE") "/Projects"))
    (setq default-directory "~/Projects")
)

;; Set limit on reading process output
;; default is 4k
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; lsp-mode can be compiled in 2 modes plist and hash-table based lsp-use-plists flag.
;; plists provide better performance in deserialization and also put less presure than hash-tables
;; This would require export LSP_USE_PLISTS=true at the time of installation of LSP package
;; TODO: Consider adding this.

(setq tramp-default-method "ssh")

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)

(add-to-list 'default-frame-alist '(font . "monospace 10"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sanityinc-tomorrow-night t)

(require 'nyan-mode)
(defun turbo_mack/init-nyan-mode ()
  "Starts nyan mode and setup animation nad wavy trails."

  (nyan-mode t)
  (nyan-start-animation)
  (nyan-toggle-wavy-trail))

(turbo_mack/init-nyan-mode)

;;; setup line spacing
(setq-default line-spacing 2)
(setq-default left-fringe-width  5)
(setq-default right-fringe-width  2)

(setq linum-format " %d ")

(set-frame-parameter (selected-frame) 'internal-border-width 0)

;; purple comments
(set-face-foreground 'font-lock-comment-face "#B193A6")

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
    nil 'alpha
    (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
        '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(require 'which-key)
(which-key-mode t)

(setenv "DICTIONARY" "en_GB")
(setq ispell-program-name "aspell")

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(electric-pair-mode)

;; Remove trailing witespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'editorconfig)
(editorconfig-mode t)

;; Line ends
(defun turbo_mack/dos-file-endings-p ()
  "Check if dos enconding is used."

  (string-match "dos" (symbol-name buffer-file-coding-system)))

(defun turbo_mack/find-file-check-line-endings ()
  "Force UNIX line endings."

  (when (turbo_mack/dos-file-endings-p)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))

(if (not (eq system-type 'windows-nt))
  (add-hook 'find-file-hook 'turbo_mack/find-file-check-line-endings)
)

(require 'dap-cpptools)

(setq
  lsp-keymap-prefix "C-c l"
  lsp-ui-doc-enable nil
  )

(require 'yasnippet)
(yas-global-mode 1)

(require 'compile)

(defun turbo_mack/my_c ()
  (interactive)
  "setup compile command"
  (set (make-local-variable 'compile-command) "make"))


(add-hook 'c-mode-hook 'turbo_mack/my_c)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c++-mode)

(require 'scss-mode)

(require 'js2-mode)
(require 'npm-mode)
(require 'nodejs-repl)
(require 'handlebars-mode)

;; enable flycheck
(add-hook 'js2-mode-hook
        (lambda () (flycheck-mode t)))

(defun turbo_mack/init-js-bindings ()
  "Setup key binding for JavaScript major mode."
  (interactive)
  (local-set-key (kbd "C-x C-e") 'nodejs-repl-send-last-sexp))

(add-hook 'js2-mode 'turbo_mack/init-js-bindings)

;; npm install -g typescript
;; npm install -g tide

(require 'typescript-mode)
(require 'tide)

;; Turn on typescript-mode for tsx files
(add-to-list 'auto-mode-alist
              '("\\.tsx\\'" . typescript-mode) t)

(defun turbo_mack/init-tide-mode ()
  "Setup tide (typescript syntax checker)."

  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode t))

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'turbo_mack/init-tide-mode)
;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(require 'elm-mode)
(add-hook 'elm-mode-hook #'elm-indent-mode)
(add-hook 'elm-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'elm-mode-hook #'lsp)

;; [[https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md][There]] is a great article about setting up Emacs for Haskell development by Serras.

;; This configuration is using several packages which requires installed binaries:

;; - [[https://github.com/chrisdone/hindent][hindent-mode]] takes care of formatting
;; - [[https://github.com/marcotmarcot/hasktags][hasktags]] is tool for creating tag files
;; - [[https://github.com/jaspervdj/stylish-haskell][stylish-haskell]] is another code formatting tool
;; - [[https://hackage.haskell.org/package/fourmolu][fourmolu]] is another code formatting tool

(require 'haskell-mode)
(require 'hindent)
(load-library "ormolu")

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-tags-on-save nil)
  ;;'(haskell-process-type 'cabal-new-repl)
  '(haskell-process-log t)
  '(ormolu-process-path "fourmolu"))

(defun turbo_mack/haskell-setup ()
    "setup haskell specific configuration"

    ;; (make-local-variable 'tab-stop-list)
    ;; (setq tab-stop-list (number-sequence 0 120 4))
    ;; (setq indent-line-function 'tab-to-tab-stop)
    (setq haskell-indent-spaces 4))

;; (add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook #'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'turbo_mack/haskell-setup)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;; Fix path
;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;   (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))


(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map (kbd "C-c C-f") 'ormolu-format-buffer)))

(eval-after-load 'haskell-cabal '(progn
                                    (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

  ;; LSP based haskell intergration
  ;; (require 'lsp)
  ;; (require 'lsp-haskell)
  ;; (add-hook 'haskell-mode-hook #'lsp)

  ;; (add-hook 'haskell-mode-hook 'lsp)
  ;; (add-hook 'haskell-mode-hook 'direnv-update-environment)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)
;;; To get completions in the REPL
(add-hook 'haskell-interactive-mode-hook 'company-mode)

;; purescript
(add-hook 'purescript-mode-hook #'purescript-indent-mode)

(require 'yaml-mode)

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-hook #'robe-mode-hook #'ac-robe-setup)


(require 'rust-mode)

(require 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(define-key rust-mode-map (kbd "C-c C-c") 'rustic-cargo-build)
(define-key rust-mode-map (kbd "C-c C-r") 'rustic-cargo-run)


(require 'nix-mode)
(add-to-list 'auto-mode-alist
              '("\\.nix\\'" . (lambda ()
                                (nix-mode))))

(require 'smartparens)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'diminish)

(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)

;; autocomplete
(add-hook 'after-init-hook 'global-company-mode)

;; Setting out company idle mode
;; shen set too high it migth overload a backend and make
;; it effectively slower!!!
(setq company-idle-delay 0.0)


;; fic-mode for highlightng TODOs, FIXMEs etc.
;; TODO: add custom face
(require 'fic-mode)
(custom-set-variables
  '(fic-highlighted-words '("FIXME" "TODO" "BUG" "HACK")))

;; turn on fic-mode
(add-hook 'prog-mode-hook 'fic-mode)


;; EVIL

(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)
(setq evil-want-C-u-scroll t)
(setq evil-undo-system 'undo-fu)
(setq evil-want-keybinding nil)

(require 'evil)
(evil-mode t)

;; dirred mode bindings
(evil-collection-init 'dired)

(defun turbo_mack/map-basic-evil-commands ()
  "Setup W/Q commands."

  (evil-ex-define-cmd "W[rite]" 'evil-write)
  (evil-ex-define-cmd "Q[uit]" 'evil-quit)
  ;; TODO: should both write and quit perhaps?
  (evil-ex-define-cmd "wq[rite]" 'evil-write)
  (evil-ex-define-cmd "Wq[rite]" 'evil-write)
  (evil-ex-define-cmd "WQ[rite]" 'evil-write))

(turbo_mack/map-basic-evil-commands)

(require 'evil-commentary)
(evil-commentary-mode t)

(require 'evil-surround)
(global-evil-surround-mode t)

(require 'evil-visualstar)
(global-evil-visualstar-mode t)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(defun turbo_mack/evil-shift-left-visual ()
  "Move selected block to left."

  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun turbo_mack/evil-shift-right-visual ()
  "Move selected block to right."

  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(define-key evil-visual-state-map (kbd ">") 'turbo_mack/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'turbo_mack/evil-shift-left-visual)
(define-key evil-visual-state-map [tab] 'turbo_mack/evil-shift-right-visual)
(define-key evil-visual-state-map [S-tab] 'turbo_mack/evil-shift-left-visual)

;; HELM

(defun turbo_mack/init-helm ()
  "Init helm."

  (helm-mode t)
  (helm-autoresize-mode t)
  ;; This will set header bg color to dark gray
  (set-face-attribute 'helm-source-header nil :background "#141414" :foreground "#f8f8f8"))

(turbo_mack/init-helm)

(require 'projectile)

(defun turbo_mack/init-projectile ()
  "Initialize projectlile."

  (setq projectile-enable-caching -1)
  (projectile-global-mode t))

(turbo_mack/init-projectile)

;; Setup Helm-Projectile integration
(require 'helm-projectile)
(setq helm-projectile-fuzzy-match t)

(require 'helm-ag)

;; MAGIT

(require 'magit)
(evil-collection-init 'magit)
(define-key evil-normal-state-map (kbd "C-g") 'magit-status)

;; Perspective

(require 'perspective)
(persp-mode t)

(require 'org)
(require 'ox)
(require 'ob)
(require 'flyspell)
(require 'evil-org)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; setup TODOs
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")))

;; projectile invalidate cache
(global-set-key (kbd "C-c i") 'projectile-invalidate-cache)

;; bulet mode
(add-hook 'org-mode-hook 'org-bullets-mode)

;; setup spell-checking
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

;; Grammar niceness
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

;; log done items
(setq org-log-done t)

;; setup faces
'(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :weight bold :height 1.3))))
'(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :weight bold :height 1.2))))
'(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :weight bold :height 1.15))))
'(org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :weight bold :height 1.1))))
'(org-level-5 ((t (:inherit variable-pitch :foreground "#2aa198" :weight bold))))
'(org-level-6 ((t (:inherit variable-pitch :foreground "#6c71c4" :weight bold))))
'(org-level-7 ((t (:inherit variable-pitch :foreground "#d33682" :weight bold))))
'(org-level-8 ((t (:inherit variable-pitch :foreground "#dc322f" :weight bold))))

;; Save org files to Dropbox
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/agenda.org"))
(setq org-agenda-files (list (concat org-directory "/agenda.org")))


(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)




;; BINDINGS

  (defun turbo_mack/vsplit-and-skip()
    "split verticaly and skip to new window."

    (interactive)
    (evil-window-vsplit)
    (windmove-right))

  (defun turbo_mack/split-and-skip()
    "split horizontaly and skip to new window."

    (interactive)
    (evil-window-split)
    (windmove-down))

  (defun turbo_mack/rotate-windows-helper(x d)
    (if (equal (cdr x) nil) (set-window-buffer (car x) d)
      (set-window-buffer (car x) (window-buffer (cadr x))) (turbo_mack/rotate-windows-helper (cdr x) d)))

  (defun turbo_mack/rotate-windows ()
    "Rotate Emacs windows."

    (interactive)
    (turbo_mack/rotate-windows-helper (window-list) (window-buffer (car (window-list))))
    (select-window (car (last (window-list)))))

  "Window navigation"
  (define-key evil-motion-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-motion-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-motion-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-motion-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-motion-state-map (kbd "C-w r") 'turbo_mack/rotate-windows)

  "Window spliting"
  (define-key evil-window-map (kbd "v") 'turbo_mack/vsplit-and-skip)
  (define-key evil-window-map (kbd "s") 'turbo_mack/split-and-skip)

  "Window resizing"
  (define-key evil-motion-state-map (kbd "C-=") 'enlarge-window-horizontally)
  (define-key evil-motion-state-map (kbd "C--") 'shrink-window-horizontally)

  "Resize text"
  (define-key evil-motion-state-map (kbd "C-+") 'text-scale-increase)
  (define-key evil-motion-state-map (kbd "C-_") 'text-scale-decrease)

  "Winner mode"
  ;;(define-key evil-normal-state-map (kbd "C-c l") 'winner-redo)
  ;;(define-key evil-normal-state-map (kbd "C-c h") 'winner-undo)

  "Org mode"
  (define-key evil-normal-state-map (kbd "C-M-l") 'org-do-demote)
  (define-key evil-normal-state-map (kbd "C-M-h") 'org-do-promote)

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (defun turbo_mack/minibuffer-keyboard-quit ()
    "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."

    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  "ESC to quit"
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'turbo_mack/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'turbo_mack/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'turbo_mack/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'turbo_mack/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'turbo_mack/minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)

  (require 'key-chord)

  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  ;; helm vim like
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key evil-motion-state-map (kbd "C-x b") 'helm-buffers-list)
  (define-key evil-motion-state-map (kbd "C-x r b") 'helm-bookmarks)
  (define-key evil-motion-state-map (kbd "C-x y") 'helm-show-kill-ring)
  (define-key evil-motion-state-map (kbd "C-x C-f") 'helm-find-files)

  (define-key evil-motion-state-map (kbd "C-c C-p") 'helm-projectile-switch-project)
  (define-key evil-motion-state-map (kbd "SPC") 'persp-switch)
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)



;; Autocomplete
  (eval-after-load 'company
    '(progn
      (define-key company-active-map (kbd "C-j") 'company-select-next)
      (define-key company-active-map (kbd "C-k") 'company-select-previous)))

;; cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; term
(defun turbo_mack/toggle-term()
  "Splits window and open terminal."

  (interactive)
  (split-window-below)
  (windmove-down)
  (term "/usr/bin/zsh"))

(define-key evil-normal-state-map (kbd "C-t") 'turbo_mack/toggle-term)


;; LINUX

(if (eq system-type 'windows-nt)
  (print "Skipping SSH on Windows")
  (when window-system
      (exec-path-from-shell-copy-env "SSH_AGENT_PID")
      (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
)


;; MACOS (not used much)

(if (eq system-type 'darwin)
  ((exec-path-from-shell-initialize)
  (when (fboundp 'osx-clip-board-mode)
            (set-face-attribute 'default nil :height 120)
            (osx-clip-board-mode t)
            (exec-path-from-shell-initialize))
  (setq mac-command-modifier 'C))
)

(provide 'init)
;;; init.el ends here
