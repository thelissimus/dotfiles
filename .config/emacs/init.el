;;; init.el --- イーマックスのすべて -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Mononoki" :height 180 :weight 'semi-bold)
(setq-default line-spacing 0.2)
(setq-default truncate-lines t)
(setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq ring-bell-function 'ignore)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)

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

(setq-default fill-column 120)
(global-display-fill-column-indicator-mode 1)

(setq-default show-trailing-whitespace t)
(setq-default whitespace-style '(face trailing tabs tab-mark spaces space-mark))
(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq create-lockfiles nil)

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
(projectile-mode 1)

(require 'perspective)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-x x"))
(customize-set-variable 'persp-state-default-file "~/.emacs.perspective")
(add-hook 'kill-emacs-hook #'persp-state-save)
(persp-mode 1)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (file-exists-p persp-state-default-file)
              (persp-state-load persp-state-default-file))))
(require 'persp-projectile)

(require 'nerd-icons)
(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

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
(column-number-mode 1)
(doom-modeline-mode 1)

(require 'olivetti)
(setq-default olivetti-body-width 140)

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(customize-set-variable 'highlight-indent-guides-method 'character)
(customize-set-variable 'highlight-indent-guides-auto-character-face-perc 50)
(setq highlight-indent-guides-highlighter-function
      (lambda (lvl resp disp)
        (and (< 0 lvl) (highlight-indent-guides--highlighter-default lvl resp disp))))

(require 'hl-todo)
(setq hl-todo-keyword-faces
      '(("TODO" . (:inherit warning :inverse-video t))
        ("WARNING" . (:inherit warning :inverse-video t))
        ("FIXME" . (:inherit error :inverse-video t))
        ("HACK" . (:inherit font-lock-constant-face :inverse-video t))
        ("NOTE" . (:inherit success :inverse-video t))))
(global-hl-todo-mode 1)

(require 'envrc)
(define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
(envrc-global-mode 1)

(require 'super-save)
(super-save-mode 1)
(setq super-save-auto-save-when-idle t)
(setq super-save-idle-duration 1)

;; git
(require 'magit)
(setq magit-diff-refine-hunk 'all)
(global-set-key (kbd "C-c g") 'magit-status)
(require 'diff-hl)
(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(setq diff-hl-show-staged-changes nil)
(setq diff-hl-highlight-reference-function nil)
(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
(require 'difftastic)
(require 'vundo)
(setq vundo-compact-display t)
(setq vundo-glyph-alist vundo-unicode-symbols)
(evil-define-key 'normal 'global (kbd "g u") 'vundo)
(setq undo-limit (* 80 1024 1024))
(setq undo-strong-limit (* 120 1024 1024))
(setq undo-outer-limit (* 360 1024 1024))

(require 'winum)
(winum-mode 1)
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
(global-wakatime-mode 1)

(require 'vertico)
(vertico-mode 1)
(savehist-mode 1)
(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles partial-completion))))
(setq completion-category-defaults nil)

(require 'marginalia)
(marginalia-mode 1)

(require 'consult)
(require 'consult-todo)
(require 'consult-hoogle)
(global-set-key (kbd "C-x b") #'consult-buffer)
(consult-customize consult--source-buffer :hidden t :default nil)
(add-to-list 'consult-buffer-sources persp-consult-source)

(require 'embark)
(require 'embark-consult)
(global-set-key (kbd "C-c s") 'consult-ripgrep)
(global-set-key (kbd "C-c C-o") 'embark-export)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'eglot)
(require 'eglot-booster)
(setq eglot-booster-io-only t)
(setq flymake-show-diagnostics-at-end-of-line t)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
(eglot-booster-mode 1)

(require 'eldoc-box)
(setq eldoc-box-clear-with-C-g t)
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
(evil-define-key 'normal 'global (kbd "g h") 'eldoc-box-help-at-point)

(require 'company)
(setq company-backends '((company-capf company-dabbrev-code)))
(setq company-minimum-prefix-length 1)
(setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.1)))
(add-hook 'after-init-hook 'global-company-mode)

(require 'paredit)
(require 'enhanced-evil-paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'paredit-mode-hook #'enhanced-evil-paredit-mode)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))
(with-eval-after-load 'agda2-mode
  (setq agda2-highlight-face-groups 'default-faces)
  (evil-define-key '(normal insert) agda2-mode-map
    (kbd "M-.") 'agda2-goto-definition-keyboard
    (kbd "M-,") 'agda2-go-back))

(setq treesit-font-lock-level 4)

(require 'nix-ts-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
(add-hook 'nix-ts-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))

(setq haskell-ts-use-indent t)
(require 'haskell-ts-mode)
(add-hook 'haskell-ts-mode-hook #'eglot-ensure)
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-ts-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-ts-mode))

(add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp)))

(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
(add-to-list 'interpreter-mode-alist '("sh" . bash-ts-mode))
(add-to-list 'interpreter-mode-alist '("bash" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(require 'just-ts-mode)
(add-to-list 'auto-mode-alist '("/[Jj]ustfile\\'" . just-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(require 'org)
(setq org-directory "~/Documents/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory))
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  SCHEDULED: %t\n  %i")

        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\n  %U\n  %i")))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (visual-line-mode 1)))

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
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  (reusable-frames . visible)
                  (window-height . 0.4))))

(load-theme 'gruvbox-dark-medium t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
(load "~/.emacs.d/local.el" t)

;;; init.el ends here
