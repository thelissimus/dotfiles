(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Mononoki" :height 170 :weight 'semi-bold)
(setq-default line-spacing 0.2)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq ring-bell-function 'ignore)

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

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

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

(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(evil-mode 1)
(evil-collection-init)

(require 'treemacs)
(require 'treemacs-evil)
(require 'treemacs-projectile)
(require 'treemacs-nerd-icons)
(setq treemacs-project-follow-mode t)
(treemacs-load-theme "nerd-icons")
(global-set-key (kbd "C-c t") #'treemacs)
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))

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

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook #'eglot-ensure)

(with-eval-after-load 'vterm
  (setq vterm-max-scrollback 10000))

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
