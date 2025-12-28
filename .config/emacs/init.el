(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Mononoki" :height 170 :weight 'semi-bold)
(setq-default line-spacing 0.2)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(setq confirm-kill-emacs 'yes-or-no-p)

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

(require 'envrc)
(define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
(envrc-global-mode)

(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(evil-mode 1)
(evil-collection-init)

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
