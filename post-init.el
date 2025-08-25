;;; --- Post Init file -*- no-byte-compile: t; lexical-binding: t; -*-


;; === Load Compile Angel first, so everything else goes faster ================

;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
  ;; the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

(setopt use-package-hook-name-suffix nil)


;; === My Functions ============================================================

(defun my/copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'.
    From https://gist.github.com/kristianhellquist/3082383"
  (interactive)
  (let ((path-with-line-number
         (concat (dired-replace-in-string (getenv "HOME") "~" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

;; Function to install missing treesitter grammars
;; source: https://github.com/renzmann/treesit-auto/issues/128#issuecomment-2637842635
(defun my/install-treesit-grammars ()
  "Install any missing tree-sitter grammars.
    From https://github.com/renzmann/treesit-auto/issues/128#issuecomment-2637842635"
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

(defun my/native-recompile ()
  "Prune eln cache and native recompile everything on `package-user-dir'.
    From https://www.reddit.com/r/emacs/comments/1gmjpn1/comment/lw4kgya"
  (interactive)
  (native-compile-prune-cache)
  (native-compile-async package-user-dir 'recursively))

(defun my/open-shell ()
  (interactive)
  (evil-window-split 12)
  (shell)
  )


;; === Customizations ===================================================================
;;
;; Place before everything else because init files are executed sequentially.
;; Read: custom-safe-themes should execute before setting themes, otherwise have to approve it with 'y'.
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6bf350570e023cd6e5b4337a6571c0325cec3f575963ac7de6832803df4d210a" "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4" "6fbe13f5f21eb3e959edfaa0185301d15309224116cc5e6f0ab3b2a40ee3bd3b"
     "8717434774f34f325aca6fedb24b572026a0e61dca6e3fe5c03f8c3af8f412f6" default))
 '(package-selected-packages
   '(auto-dark compile-angel corfu dashboard dired-sidebar dirvish doom-modeline eldoc-box emacs-lisp evil-collection helpful htmlize indent-bars magit
               marginalia markdown-mode modus-themes nerd-icons-completion nerd-icons-corfu nerd-icons-dired nerd-icons-ibuffer orderless org-appear
               org-modern org-reverse-datetree pet prog-mode tabspaces treesit-auto vertico visual-fill-column yaml-pro))
 '(use-package-compute-statistics t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; === use-package =============================================================

(use-package use-package
  :ensure nil
  :custom
  (use-package-hook-name-suffix nil)
  (use-package-always-ensure t)
  (use-package-co)
  )

;; === Emacs ===================================================================

(use-package emacs
  :ensure nil

  :hook
  (after-init-hook . global-auto-revert-mode)
  (after-init-hook . save-place-mode)
  (after-init-hook . savehist-mode)
  (after-init-hook . (lambda()
                       (let ((inhibit-message t))
                         (recentf-mode 1))))
  (kill-emacs-hook . recentf-cleanup)

  :bind (
         ([escape] . keyboard-quit)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("C-`" . my/open-shell)
         :map minibuffer-local-map
         ([escape] . abort-minibuffers)
         )

  :config
  (if (eq system-type 'darwin)
      (setopt insert-directory-program "gls"))
  (if (eq system-type 'windows-nt)
      (setopt shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe"))
  (if (eq system-type 'windows-nt)
      (set-fontset-font t 'symbol "Segoe UI Symbol")
    )
  (set-face-attribute 'default nil :height 120)
  (cond
   ((find-font (font-spec :name "Hack Nerd Font Mono"))
    (set-face-attribute 'default nil :font "Hack Nerd Font Mono")
    (set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font Mono" )))
  (cond
   (
    (find-font (font-spec :name "Atkinson Hyperlegible Next"))
    (set-face-attribute 'variable-pitch nil :font "Atkinson Hyperlegible Next")
    )
   )

  :custom
  (user-full-name "Luke D Russell")
  (user-mail-address "LukeDRussell+git@outlook.com")
  (display-line-numbers-type 'relative)
  (scroll-margin 5)
  (dired-kill-when-opening-new-dired-buffer t)
  (package-install-upgrade-built-in t)
  )

;; === Colour themes ============================================================

(use-package modus-themes
  :defer t
  :ensure nil
  :custom
  (modus-themes-common-palette-overrides
   '((bg-line-number-active unspecified)
     (bg-line-number-inactive unspecified))
   ))

(use-package auto-dark
  :config (auto-dark-mode)
  :custom
  (auto-dark-light-theme 'modus-operandi)
  (auto-dark-dark-theme 'modus-vivendi)
  )


;; === Visuals ==================================================================

(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner 'official)
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-items '((recents . 10) (projects . 10) (bookmarks . 10)))
  (dashboard-startupify-list
   '(dashboard-insert-banner dashboard-insert-newline dashboard-insert-banner-title dashboard-insert-newline dashboard-insert-init-info
                             dashboard-insert-items dashboard-insert-newline))
  )

(use-package doom-modeline
  :config (doom-modeline-mode)
  :after evil
  :custom
  (doom-modeline-buffer-file-name truncate-upto-project)
  (doom-modeline-modal-icon nil)
  (doom-modeline-buffer-encoding nil)
  (display-time-mode t)
  ;; Don't duplicate Mode in messages, it's already in the modeline.
  (evil-insert-state-message nil)
  (evil-visual-state-message nil)
  ;; Customize the Mode labels to how they usually are in the bottom line thingie
  (evil-normal-state-tag " NORMAL ")
  (evil-insert-state-tag " INSERT ")
  (evil-visual-state-tag " VISUAL ")
  :custom-face
  (doom-modeline-evil-insert-state
   ((t (:background "olive drab" :foreground "white smoke"))))
  (doom-modeline-evil-visual-state
   ((t (:background "medium slate blue" :foreground "white smoke")))))

(use-package visual-fill-column
  ;; Centres text when the window is really wide. I don't like turning my head left to read the
  ;; text of ultrawide frames.
  :config
  (global-visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (fill-column 150))

(use-package nerd-icons
  :defer t
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode)
  )

(use-package nerd-icons-completion
  :after marginalia
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
  )

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(use-package indent-bars
  :hook ((python-base-mode yaml-ts-mode) . indent-bars-mode))


;; === Help =====================================================================

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-display-columns 5)
  (which-key-add-column-padding 10)
  )

(use-package marginalia ;; Docstrings in minibuffer margin
  :after vertico
  :init (marginalia-mode))

(use-package helpful
  :defer t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

;; === Modal Editing=============================================================

(use-package evil
  :after which-key
  :init
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)
  :custom
  (evil-set-undo-system 'undo-redo)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))
  (which-key-add-key-based-replacements
    "<leader> b" "buffers"
    "<leader> e" "emacs"
    "<leader> h" "help"
    "<leader> o" "open"
    "<leader> q" "quit"
    )
  (evil-define-key nil 'global
    (kbd "<leader> bi") '("ibuffer" . ibuffer)
    (kbd "<leader> bn") '("Next buffer" . evil-next-buffer)
    (kbd "<leader> bp") '("Prev buffer" . evil-prev-buffer)
    (kbd "<leader> bs") '("Switch buffer" . switch-to-buffer)
    (kbd "<leader> bk") '("Kill current buffer" . kill-current-buffer)
    (kbd "<leader> bK") '("Kill a buffer" . kill-buffer)
    (kbd "<leader> bP") '("Project switch buffer" . project-switch-to-buffer)

    (kbd "<leader> ec") '("open user config" . (lambda () (interactive) (find-file user-init-file)))
    (kbd "<leader> eo") '("emacs config options" . (lambda () (interactive) (helpful-variable 'system-configuration-options)))
    (kbd "<leader> ep") '("packages" . list-packages)
    (kbd "<leader> eu") '("show packages that can be upgraded" . package-menu-filter-upgradable)

    (kbd "<leader> ha") '("apropos" . apropos)
    (kbd "<leader> h@") '("at point" . eldoc-box-help-at-point)
    (kbd "<leader> hc") '("describe command" . helpful-command)
    (kbd "<leader> he") '("emacs manual" . info-emacs-manual)
    (kbd "<leader> hf") '("function" . helpful-function)
    (kbd "<leader> hF") '("Face" . describe-face)
    (kbd "<leader> hk") '("key" . helpful-key)
    (kbd "<leader> hm") '("manuals" . info-display-manual)
    (kbd "<leader> hM") '("macro" . helpful-macro)
    (kbd "<leader> ho") '("org-mode manual" . org-info)
    (kbd "<leader> hq") '("quick help" . help-quick-toggle)
    (kbd "<leader> hs") '("symbol" . helpful-symbol)
    (kbd "<leader> hu") '("use-package manual" . (lambda () (interactive) (info-display-manual "use-package")))
    (kbd "<leader> hv") '("variable" . helpful-variable)
    (kbd "<leader> hw") '("which-key" . which-key-show-top-level)

    (kbd "<leader> wv") '("vertical new" . evil-window-vnew)
    (kbd "<leader> wh") '("horizontal new" . evil-window-new)
    (kbd "<leader> wc") '("close window" . evil-window-delete)
    (kbd "<leader> wm") '("maximise current" . delete-other-windows)
    (kbd "<leader> wr") '("rotate" . evil-window-rotate-upwards)
    (kbd "<leader> wT") '("Tear off" . tear-off-window)
    (kbd "<leader> w<up>") '("up" . evil-window-up)
    (kbd "<leader> w<down>") '("up" . evil-window-down)
    (kbd "<leader> w<left>") '("up" . evil-window-left)
    (kbd "<leader> w<right>") '("up" . evil-window-right)

    (kbd "<leader> om") '("magit" . magit)
    (kbd "<leader> od") '("dired" . dired-sidebar-toggle-sidebar)
    (kbd "<leader> oD") '("Dashboard". dashboard-open)

    (kbd "<leader> qr") '("restart" . restart-emacs)
    (kbd "<leader> qq") '("quit" . save-buffers-kill-emacs)
    )
  )

;; 
;;    "o f" '(consult-find :wk "file")
;;    "o a" '(org-agenda :wk "agenda")
;;    "o T" '(tabspaces-open-or-create-project-and-workspace :wk "Tabspace"
;; 
;;    "q" '(:ignore t :wk "quit")
;;    "q r" '(restart-emacs :wk "Restart emacs")
;;    "q n" '(restart-emacs-start-new-emacs :wk "restart to New emacs")
;;    "q q" '(save-buffers-kill-terminal :wk "Quit emacs")
;; 
;;    "u" '(:ignore t :wk "ui")
;;    "u m" '(toggle-menu-bar-mode-from-frame :wk "Menu bar")
;;    "u M" '(org-modern-mode :wk "Org-Modern mode")
;;    "u l" '(lr/cycle-line-number-style :wk "Line numbers")
;;    "u F" '(toggle-frame-fullscreen :wk "Fullscreen")
;;    "u t" '(consult-theme :wk "theme preview / change")

(use-package evil-collection
  :after evil
  :config (evil-collection-init))


;; === Version Control =========================================================

(use-package transient
  :defer t)

(use-package magit
  :defer t)


;; === Tooling ==================================================================

(use-package corfu
  ;; Intellisense-style completion popups
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary)
  )

(use-package vertico
  ;; Minibuffer completion UI
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package orderless
  ;; Stop caring about the order of search terms in minibuffer filtering
  :custom
  (completion-styles '(orderless basic)) (completion-category-overrides
	                                      '((file (styles basic partial-completion)))))
<<<<<<< HEAD
(use-package dired-sidebar
  :commands
  (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'nerd-icons)
  (dired-sidebar-should-follow-file t)
  (dired-sidebar-follow-file-timer 0.02)
  (dired-sidebar-display-alist '((side . right) (slot . -1)))
)
=======

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )
>>>>>>> ca1bd63 (Updates on Linux, using minimal emacs)

(use-package tabspaces)

;; === IDE ======================================================================

;; Auto install and use all tree-sitter grammars
;; Run =treesit-auto-install-all= to install the grammars
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package eglot
  :hook
  (python-base-mode-hook . eglot-ensure)
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode-hook . hs-minor-mode)
  (prog-mode-hook . display-line-numbers-mode)
  (prog-mode-hook . electric-pair-mode)
)

(use-package yaml
  :defer t
  :hook
  (yaml-mode-hook . hs-minor-mode))

(use-package yaml-pro
  :hook (yaml-mode-hook . yaml-pro-mode)
  :defer t)

;; Markdown
(use-package markdown-mode
  :defer t
  :init (setq markdown-command "multimarkdown")
  )

;; Python
(use-package pet
  :defer t
  :hook (python-base-mode-hook . pet-mode)
  )

(use-package eldoc-box
  :defer t)

(use-package elisp-mode
  :ensure nil)

;; === Org and Calendar =========================================================

(use-package calendar
  :defer t
  :custom
  (calendar-date-style 'iso)
  (calendar-week-start-day 1)
  (calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
     'font-lock-face 'font-lock-function-name-face)
   )
  (calendar-intermonth-header "Wk")
  (calendar-left-margin 10)
  (calendar-intermonth-spacing 10)
  )

(use-package org
  :defer t
  :hook (org-mode-hook . visual-line-mode)
  :bind (:map org-mode-map ("C-L" . org-store-link))
  :custom
  (org-directory "~/Notes/")
  (org-agenda-files (list org-directory))
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-archive-location "::* Archive")
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-id-link-to-org-use-id t)
  (org-startup-with-inline-images t)
  (org-pretty-entities t)
  (org-babel-load-languages '((lua . t) (python . t) (shell . t) (emacs-lisp . t)))	
  (org-todo-keywords
   '((sequence "TODO(t!)" "SOMEDAY(s!)" "WIP(i!)" "WAITING(w@)" "|" "DONE(d!)" "MOVED(m@)" "CANCELLED(c@)")))
  )

(use-package org-appear
  :hook
  (org-mode-hook . org-appear-mode)
  (org-mode-hook . (lambda ()
                     (add-hook 'evil-insert-state-entry-hook
                               #'org-appear-manual-start
                               nil
                               t)
                     (add-hook 'evil-insert-state-exit-hook
                               #'org-appear-manual-stop
                               nil
                               t)))
  :custom
  (org-appear-trigger 'manual)
  )

(use-package org-modern
  :hook (org-mode-hook . global-org-modern-mode)
  :custom
  (org-modern-hide-stars " ")
  )

(use-package org-reverse-datetree
  :defer t
  :custom
  ;; Check function =format-time-string= for syntax.
  (org-reverse-datetree-level-formats
   '("Journal"
     "%Y"                                                                               ; year
     (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time))) ; month
     "%Y W%V"                                                                           ; week
     "%Y-%m-%d %A"                                                                      ; date
     ))
  )

(use-package htmlize
  :defer t)

