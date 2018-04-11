(load "~/.emacs.d/secrets.el")

(setq user-full-name "Marek Fajkus"
      user-mail-address "marek.faj@gmail.com")

(setq tramp-default-method "ssh")

(exec-path-from-shell-initialize)

(require 'server)
(unless (server-running-p)
  (server-start))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (if (display-graphic-p)
      (if (memq window-system '(mac ns))
          (menu-bar-mode t)
          (menu-bar-mode -1))
    (menu-bar-mode -1)))

(setq initial-major-mode 'text-mode)

(add-hook 'prog-mode-hook 'linum-mode)

(when (display-graphic-p)
  (fringe-mode 2)
  (setq-default left-fringe-width 12)
  (setq-default right-fringe-width 0))

(setq-default truncate-lines t)
(setq line-spacing 3)
(set-face-attribute 'default nil :height 120)

(run-with-idle-timer (* 60 3) t #'garbage-collect)

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
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'fogus t)
(set-face-background 'hl-line "#000000")

(require 'nyan-mode)
(defun turbo_mack/init-nyan-mode ()
  "Starts nyan mode and setup animation nad wavy trails."

  (nyan-mode t)
  (nyan-start-animation)
  (nyan-toggle-wavy-trail))

(when (display-graphic-p)
  "Nyan mode only in gui"

  (turbo_mack/init-nyan-mode))

;;; setup line spacing
(setq-default line-spacing 2)
(setq-default left-fringe-width  5)
(setq-default right-fringe-width  2)

(setq linum-format " %d ")
(frame-parameter (make-frame '((border-width . 5))) 'border-width)

(set-frame-parameter (selected-frame) 'internal-border-width 0)

(set-frame-parameter (selected-frame) 'alpha '(95 80))
(add-to-list 'default-frame-alist '(alpha 95 80))

(require 'which-key)
(which-key-mode t)

(require 'smartparens)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'diminish)

(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)

;; autocomplete
(add-hook 'after-init-hook 'global-company-mode)

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

(add-hook 'find-file-hook 'turbo_mack/find-file-check-line-endings)

(require 'scss-mode)

(require 'nvm)
(require 'npm-mode)
(require 'nodejs-repl)
(require 'skewer-mode)
(require 'ember-mode)
(require 'handlebars-mode)

;; enable flycheck
(add-hook 'js-mode-hook
        (lambda () (flycheck-mode t)))

(defun turbo_mack/init-js-bindings ()
  "Setup key binding for JavaScript major mode."
  (interactive)
  (local-set-key (kbd "C-x C-e") 'nodejs-repl-send-last-sexp))

(add-hook 'js-mode 'turbo_mack/init-js-bindings)

(require 'typescript-mode)
(require 'tide)

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


;; auto complete
(eval-after-load 'company
  '(push 'company-elm company-backends))

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'elm-mode-hook #'elm-oracle-setup-ac)

(defun turbo_mack/init-elm ()
  (elm-tags-on-save t)
  (elm-format-on-save t)
)

;; seems to be broken
;; (add-hook 'elm-mode-hook 'turbo_mack/init-elm)

(require 'haskell-mode)
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(custom-set-variables '(haskell-tags-on-save t))

;; use stack instead of plain ghci
(custom-set-variables '(haskell-process-type 'stack-ghci))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(add-hook 'purescript-mode-hook #'haskell-indentation-mode)

(require 'rbenv)
(rbenv-use-corresponding)

;; Rails
(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'yaml-mode)

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-hook #'robe-mode-hook #'ac-robe-setup)

(require 'markdown-mode+)

;; Grammar niceness
(add-hook 'markdown-mode
          (lambda ()
            (writegood-mode)))

(require 'erlang)
(require 'erlang-start)

(require 'elixir-mode)
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

;; setup smartprens
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(require 'clojure-mode)
(require 'cider)
(require 'flycheck-clojure)

(require 'rust-mode)

(require 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(require 'flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(require 'restclient)

(require 'flycheck)
(global-flycheck-mode t)

(require 'auto-complete)
(require 'auto-complete-config)

(defun turbo_mack/init-auto-complete ()
  "Setup auto compltete mode."

  (setq ac-auto-show-menu t)
  (setq ac-auto-start t)

  (setq ac-quick-help-delay 0.3)
  (setq ac-quick-help-height 30)
  (setq ac-show-menu-immediately-on-auto-complete t))

(turbo_mack/init-auto-complete)

(defun turbo_mack/init-ac-etags ()
  "Setup auto completion for etags."
  (setq ac-etags-requires t)
  (ac-config-default)
  (ac-etags-setup))

(turbo_mack/init-ac-etags)

(defun turbo_mack/init-evil ()
  "Setup evil... Very magicaly."

  (evil-mode t)
  (setq evil-search-module 'evil-search)
  (setq evil-magic 'very-magic))
  (setq evil-want-C-u-scroll t)

(turbo_mack/init-evil)

(defun turbo_mack/map-basic-evil-commands ()
  "Setup W/Q commands."

  (evil-ex-define-cmd "W[rite]" 'evil-write)
  (evil-ex-define-cmd "Q[uit]" 'evil-quit)
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

;; This package doesn't need to be init manually.
;;(require 'navigate)

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

(require 'helm)
(require 'helm-config)

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

(require 'magit)
(require 'evil-magit)

(require 'perspective)
(persp-mode t)

(require 'yasnippet)

(autoload 'wl "wl" "Wanderlust" t)

(setq twittering-icon-mode t)

(require 'slack)
(setq slack-buffer-emojify t)
(setq slack-prefer-current-team t)

;; This are example binding from package README
;; I'll probably change those later
(evil-define-key 'normal slack-info-mode-map
  ",u" 'slack-room-update-messages)
(evil-define-key 'normal slack-mode-map
  ",c" 'slack-buffer-kill
  ",ra" 'slack-message-add-reaction
  ",rr" 'slack-message-remove-reaction
  ",rs" 'slack-message-show-reaction-users
  ",pl" 'slack-room-pins-list
  ",pa" 'slack-message-pins-add
  ",pr" 'slack-message-pins-remove
  ",mm" 'slack-message-write-another-buffer
  ",me" 'slack-message-edit
  ",md" 'slack-message-delete
  ",u" 'slack-room-update-messages
  ",2" 'slack-message-embed-mention
  ",3" 'slack-message-embed-channel
  "\C-n" 'slack-buffer-goto-next-message
  "\C-p" 'slack-buffer-goto-prev-message)
(evil-define-key 'normal slack-edit-message-mode-map
  ",k" 'slack-message-cancel-edit
  ",s" 'slack-message-send-from-buffer
  ",2" 'slack-message-embed-mention)

(slack-register-team
 :name "globalwebindex"
 :default t
 :client-id ""
 :client-secret ""
 :token turbo_mack/secret/slack_token_gwi
 :subscribed-channels '(team-platform platform-engineering engineering)
 :full-and-display-names t
 )

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
(define-key evil-normal-state-map (kbd "C-c l") 'winner-redo)
(define-key evil-normal-state-map (kbd "C-c h") 'winner-undo)

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

(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

(global-set-key (kbd "M-x") 'helm-M-x)
(define-key evil-motion-state-map (kbd "C-x b") 'helm-buffers-list)
(define-key evil-motion-state-map (kbd "C-x r b") 'helm-bookmarks)
(define-key evil-motion-state-map (kbd "C-x y") 'helm-show-kill-ring)
(define-key evil-motion-state-map (kbd "C-x C-f") 'helm-find-files)

(define-key evil-motion-state-map (kbd "C-o") 'helm-projectile-switch-project)
(define-key evil-motion-state-map (kbd "SPC") 'persp-switch)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

;; auto-complete
(define-key ac-mode-map (kbd "C-j") 'ac-next)
(define-key ac-mode-map (kbd "C-k") 'ac-previous)

  ;; company
(eval-after-load 'company
  '(progn
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)))

(define-key evil-normal-state-map (kbd "C-g") 'magit-status)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun turbo_mack/toggle-term()
  "Splits window and open terminal."

  (interactive)
  (split-window-below)
  (windmove-down)
  (term "/bin/zsh"))

(define-key evil-normal-state-map (kbd "C-t") 'turbo_mack/toggle-term)

(define-key evil-normal-state-map (kbd "C-S-s") 'slack-select-rooms)

(when (fboundp 'osx-clip-board-mode)
          (osx-clip-board-mode t))

(setq mac-command-modifier 'C)

(when window-system
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
