;;; init.el --- イーマックスのすべて -*- lexical-binding: t -*-

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Mononoki" :height 170 :weight 'semi-bold)
(setq-default line-spacing 0.2)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq ring-bell-function 'ignore)

;; glasses
(setq glasses-separator "-")
(setq glasses-uncapitalize-p t)

;; perf
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(setq scroll-margin 8)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)

(setq-default fill-column 100)
(global-display-fill-column-indicator-mode t)

(setq-default show-trailing-whitespace t)
(setq-default whitespace-style '(face trailing tabs tab-mark spaces space-mark))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; modal editing
(setq evil-want-keybinding nil)
(setq evil-want-fine-undo t)
(setq evil-undo-system 'undo-redo)
(require 'evil)
(require 'evil-collection)
(evil-mode 1)
(evil-collection-init)

(require 'projectile)
(setq projectile-enable-caching 'persistent)
(setq projectile-cache-file ".projectile-cache.eld.log")
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(require 'perspective)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-x x"))
(customize-set-variable 'persp-state-default-file "~/.emacs.perspective")
(add-hook 'kill-emacs-hook #'persp-state-save)
(persp-mode)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (file-exists-p persp-state-default-file)
              (persp-state-load persp-state-default-file))))
(require 'persp-projectile)

(require 'nerd-icons)

(require 'dashboard)
(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner 1)
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
(setq dashboard-projects-backend 'projectile)
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-setup-startup-hook)

(require 'doom-modeline)
(setq doom-modeline-height 31)
(setq doom-modeline-total-line-number t)
(column-number-mode t)
(doom-modeline-mode 1)

(require 'envrc)
(define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
(envrc-global-mode)

;; git
(require 'magit)
(setq magit-diff-refine-hunk 'all)
(global-set-key (kbd "C-c g") 'magit-status)
(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(require 'vundo)
(setq vundo-compact-display t)
(setq vundo-glyph-alist vundo-unicode-symbols)
(evil-define-key 'normal 'global (kbd "g u") 'vundo)

(require 'treemacs)
(require 'treemacs-evil)
(require 'treemacs-projectile)
(require 'treemacs-magit)
(require 'treemacs-perspective)
(require 'treemacs-nerd-icons)
(treemacs-project-follow-mode t)
(treemacs-follow-mode -1)
(treemacs-set-scope-type 'Perspectives)
(treemacs-load-theme "nerd-icons")
(treemacs-git-mode 'simple)
(global-set-key (kbd "C-c t") #'treemacs-add-and-display-current-project-exclusively)

(require 'winum)
(winum-mode)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)
(global-set-key (kbd "M-0") 'winum-select-window-0-or-10)

(require 'wakatime-mode)
(global-wakatime-mode)

(require 'vertico)
(vertico-mode)
(savehist-mode)
(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles partial-completion))))
(setq completion-category-defaults nil)

(require 'marginalia)
(marginalia-mode)

(require 'consult)
(global-set-key (kbd "C-x b") #'consult-buffer)

(require 'eglot)
(setq flymake-show-diagnostics-at-end-of-line t)

(require 'eldoc-box)
(setq eldoc-box-clear-with-C-g t)
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
(evil-define-key 'normal 'global (kbd "g h") 'eldoc-box-help-at-point)

(require 'company)
(setq company-backends '((company-capf company-dabbrev-code)))
(setq company-minimum-prefix-length 1)
(setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.1)))
(add-hook 'after-init-hook 'global-company-mode)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook #'eglot-ensure)

(with-eval-after-load 'vterm
  (setq vterm-max-scrollback 10000))

(add-hook 'vterm-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode -1)
            (setq show-trailing-whitespace nil)))

(global-set-key (kbd "C-`") #'vterm-toggle)
(with-eval-after-load 'vterm-toggle
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (equal major-mode 'vterm-mode))))
                  (display-buffer-reuse-window display-buffer-in-direction)
                  (direction . bottom)
                  (dedicated . t)
                  (window-height . 0.4))))

(load-theme 'gruvbox-dark-medium t)
