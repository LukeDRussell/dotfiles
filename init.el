;; -*- lexical-binding: t; -*-

;; === Package Management ===========================================================================

; Help in info-display-manual --> use-package --> index
; use (featurep 'builtin-package-name) to figure out the name of a builtin module / package / thingie
; e.g. (featurep 'use-package-core) evals to t
(require 'package)
(use-package use-package-core
    :ensure nil  ;; This package is built-in, don't try to download it.
    :init
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    :config
        (package-initialize)
    :custom
	(use-package-always-ensure t) ;; Always install packages listed
	(package-install-upgrade-built-in t)  ;; Upgrade built-in packages from package-archives.
)

; use-package support for installing from source.
; Note: was merged into emacs 2023-05-16. Should be emacs 30.
(unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)


;; === My Functions =================================================================================

(defun lr/cycle-line-number-style ()
    "Cycles through the line styles I like."
    (interactive)
    (cond
    ((eq display-line-numbers nil)
	(setq display-line-numbers t))
    ((eq display-line-numbers t)
	(setq display-line-numbers 'relative))
    ((eq display-line-numbers 'relative)
	(setq display-line-numbers nil)))
)
(defun lr/copy-current-line-position-to-clipboard ()
    "Copy current line in file to clipboard as '</path/to/file>:<line-number>'. Stolen from https://gist.github.com/kristianhellquist/3082383"
    (interactive)
    (let ((path-with-line-number
           (concat (dired-replace-in-string (getenv "HOME") "~" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
      (kill-new path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard"))))


;; === Emacs core ===================================================================================

(use-package emacs ;; For things without their own /feature/
    :ensure nil
    :hook
        (emacs-startup . toggle-frame-maximized)
        (prog-mode . hs-minor-mode) ;; Enable code folding with inbuilt hs
	(prog-mode . display-line-numbers-mode)
    :bind
        ("C-=" . text-scale-increase)
        ("C--" . text-scale-decrease)
    :config
        (tool-bar-mode 0) ;; Tool bar is ugly
        (scroll-bar-mode 0) ;; Scroll bar is also ugly
        (if (eq system-type 'darwin)
        (menu-bar-mode 1) ;; Hide menu bar in Linux and Windows
        (menu-bar-mode 0))  ;; Keep the menu bar in MacOS as it integrates with the OS top panel
        (electric-pair-mode t)
        (show-paren-mode 1)
        (savehist-mode t) ;; Save minibuffer history
        (recentf-mode t) ;; Keep track of open files
        (global-auto-revert-mode t) ;; Keep files up-to-date when they change outside Emacs
        ;; (pixel-scroll-precision-mode) ;; It's really jerky on my Macbook
        (if (eq system-type 'darwin)
            (setq insert-directory-program "gls")())
    :custom-face
        (default ((t (:height 150 ))))
    :custom
        (window-resize-pixelwise t)
        (frame-resize-pixelwise t)
        (load-prefer-newer t)
        (backup-by-copying t)
        (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
        (visible-bell t)
        (ring-bell-function 'ignore)
        (vc-follow-symlinks t) ;; Stop bugging me when opening my init.el which is a symlink.
	(use-short-answers t)
        (inhibit-startup-screen t)
        (inhibit-startup-message t)
        (inhibit-startup-echo-area-message "lrussell")
	(initial-scratch-message nil)
        (global-auto-revert-non-file-buffers t) ;; Keep dired up-to-date with files on disk
        (scroll-conservatively 101) ;; When scrolling top or bottom of window, don't recenter point
        (scroll-margin 5)
        (create-lockfiles nil) ;; Don't lock files. Causes my keyboard to restart
        (native-comp-async-report-warnings-errors "silent")
        (recentf-exclude ;; Ignore these regex paths from recent file list
            '("/opt/homebrew"
            "/usr/share/emacs/"
            "~/.config/emacs/elpa/"
            "~/.config/emacs/.cache/"
            "~/Library/CloudStorage/"
            (recentf-expand-file-name "~/.config/emacs/elpa")
            (recentf-expand-file-name "~/.config/emacs/.cache/treemacs-persist-at-last-error")
            "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")
        )
)


;; === Themes ======================================================================================
(use-package kanagawa-theme
    :defer t)
(use-package standard-themes
    :defer t)
(use-package ef-themes
    :defer t)
(use-package modus-themes
    :defer t)
(use-package lambda-themes
    :vc (:fetcher github :repo lambda-emacs/lambda-themes)
    :custom
        (lambda-themes-set-italic-comments t)
        (lambda-themes-set-italic-keywords t)
        (lambda-themes-set-variable-pitch t)
    :defer t)
(use-package solarized-theme
    :defer t)
(use-package auto-dark
    :config (auto-dark-mode t)
    :custom
        (auto-dark-light-theme 'modus-operandi)
        (auto-dark-dark-theme 'modus-vivendi)
    :defer t)


;; === Fonts ========================================================================================
(cond
 ((find-font (font-spec :name "Hack Nerd Font Mono"))
  (set-face-attribute 'default nil
                      :font "Hack Nerd Font Propo"
                      :height 140)))


;; === Visuals ======================================================================================
(use-package indent-bars
    :vc (:fetcher github :repo jdtsmith/indent-bars)
    :hook
	(prog-mode . indent-bars-mode)
    :custom
	(indent-bars-prefer-character t)
	(indent-bars-treesit-support t)
	(indent-bars-color-by-depth nil)
	(indent-bars-highlight-current-depth '(:face default :blend 0.4))
    :defer t)
 
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)
    :defer t)

(use-package breadcrumb
    ;; :init (breadcrumb-mode)
  :defer t)

(use-package dashboard
    :config
        (dashboard-setup-startup-hook)
    :custom
        (dashboard-startup-banner 'logo)
        (dashboard-banner-logo-title nil)
        (dashboard-center-content t)
        (dashboard-vertically-center-content t)
        (dashboard-icon-type 'nerd-icons)
        (dashboard-set-heading-icons t)
        (dashboard-set-file-icons t)
        (dashboard-set-footer nil)
        (dashboard-projects-backend 'project-el)
        (dashboard-display-icons-p t)
        (dashboard-items '((recents . 10) (projects . 10)))
)
(use-package mini-echo
    :config
        (mini-echo-mode)
    :custom
        (mini-echo-define-segments (
	    :long ("major-mode" "buffer-name" "vcs" "buffer-position" "flymake" "process" "selection-info" "narrow" "macro" "profiler" "repeat")
	    :short ("buffer-name-short" "buffer-position" "process" "profiler" "selection-info" "narrow" "macro" "repeat"))
	)
	)

(use-package visual-fill-column
  :config
      (global-visual-fill-column-mode)
  :custom
	(visual-fill-column-center-text t)
	(fill-column 150)
  )

;; === Workspace Management =========================================================================
(use-package tabspaces ;; Each tab is a set of isolated buffers
    :vc (tabspaces :url "https://github.com/mclear-tools/tabspaces")
    :hook (after-init . tabspaces-mode)
)
;; === Editing Support ==============================================================================
(use-package which-key ;; Discover keybinds with popup
    :after evil
    :config (which-key-mode)
    :custom
	(which-key-max-display-columns 5)
	(which-key-add-column-padding 10)
)
(use-package vertico ;; Minibuffer completion UI
    :init (vertico-mode)
    :custom
	(vertico-cycle t)
	(read-buffer-completion-ignore-case t)
	(read-file-name-completion-ignore-case t)
)
(use-package marginalia ;; Docstrings in minibuffer margin
    :after vertico
    :init (marginalia-mode)
)
(use-package corfu ;; Intellisense-style completion popups
    :init
	(global-corfu-mode)
	(corfu-popupinfo-mode)
    :custom
	(corfu-auto t)
	(corfu-cycle t)
	(corfu-quit-at-boundary)
)
(use-package orderless ;; Stop caring about the order of search terms in minibuffer filtering
    :custom
	(completion-styles '(orderless basic)) (completion-category-overrides
	    '((file (styles basic partial-completion))))
)
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
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
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
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
   consult-ripgrep consult-git-grep consult-grep
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
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)
;; === Modal editing ================================================================================
(use-package evil
    :init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
    :config
	(evil-mode 1)
	(evil-set-undo-system 'undo-redo) ;; Only because Emacs 28+ has this built in
    :custom
	(evil-split-window-below t)
	(evil-vsplit-window-right t)
)
(use-package evil-collection
    :after evil
    :config (evil-collection-init)
)
;; === Version Control Systems ======================================================================
(use-package magit)
;; === Keybinds =====================================================================================
(use-package general
    :config
	(general-evil-setup)
	(general-create-definer lr/leader-def
	;; set up 'SPC' as the global leader key
	    :states '(normal insert visual emacs)
	    :keymaps 'override
	    :prefix "SPC"	   ;; set leader
	    :global-prefix "M-SPC" ;; access leader in insert mode
	)
	;; format: off
	(lr/leader-def ;; Leader sequences
	    "b"   '(:ignore t :wk "buffers")
	    "b c" '(consult-buffer :wk "menu for buffers")
	    "b d" '(kill-buffer-and-window :wk "delete buffer")
	    "b n" '(next-buffer :wk "next buffer")
	    "b p" '(previous-buffer :wk "previous buffer")
	    "b m" '(consult-project-buffer :wk Project buffers)

	    "e"     '(:ignore t :wk "emacs")
	    "e c" '((lambda () (interactive) (find-file user-init-file)) :wk "config edit")
	    "e l" '((lambda () (interactive) (load-file user-init-file)) :wk "load config")
	    "e o"   '(describe-variable 'system-configuration-options) :wk "options used for build "
	    "e p"   '(list-packages :wk "list all pacakges")
	    "e u"   '(package-menu-filter-upgradable :wk "show packages that can be upgraded")

	    "f"   '(:ignore t :wk "files")
	    "f f" '(consult-fd :wk "find in project")
	    "f g" '(consult-ripgrep :wk "grep in project")
	    "f s" '(save-buffer :wk "save buffer")

	    "w"       '(:ignore t :wk "windows")
	    "w v"       '(evil-window-vnew :wk "vertical split")
	    "w h"       '(evil-window-new :wk "horizontal split")
	    "w d"       '(evil-window-delete :wk "delete")
	    "w n"       '(evil-window-next :wk "next")  
	    "w p"       '(evil-window-prev :wk "prev")
	    "w m"       '(delete-other-windows :wk "maximise current")
	    "w r"       '(evil-window-rotate-upwards :wk rotate)
	    "w T"       '(tear-off-window :wk "Tear off window to new frame")
	    "w <up>"    '(evil-window-up :wk "up")
	    "w <down>"  '(evil-window-down :wk "down")
	    "w <left>"  '(evil-window-left :wk "left")
	    "w <right>" '(evil-window-right :wk "right")
	    "v" '(:ignore t :wk "version control")
	    "v m" '(magit :wk "magit")
	    "v h" '(magit-log-buffer-file :wk "history of file")

	    "o"   '(:ignore t :wk "open")
	    "o v" '(vterm-toggle :wk "vterm")
	    "o d" '(dirvish :wk "open dirvish")
	    "o s" '(dirvish-side :wk "open dirvish to side")
	    "o D" '(dashboard-open :wk "dashboard")
	    "o t" '(treemacs :wk "treemacs")
	    "o m" '(magit :wk "magit")
	    "o f" '(consult-find :wk "find file")
	    "o a" '(org-agenda :wk "agenda")
	    "o p" '(tabspaces-open-or-create-project-and-workspace :wk "project in tab")

	    "q" '(:ignore t :wk "quit")
	    "q r" '(restart-emacs :wk "Restart emacs")
	    "q n" '(restart-emacs-start-new-emacs :wk "restart to New emacs")
	    "q q" '(save-buffers-kill-terminal :wk "Quit emacs")
	    "u" '(:ignore t :wk "ui")
	    "u m" '(toggle-menu-bar-mode-from-frame :wk "Menu bar")
	    "u M" '(org-modern-mode :wk "Org-Modern mode")
	    "u l" '(lr/cycle-line-number-style :wk "Line numbers")
	    "u F" '(toggle-frame-fullscreen :wk "Fullscreen")
	    "u t" '(consult-theme :wk "theme preview / change")
	    "h" '(:ignore t :wk "(h)elp")
	    "h a" '(apropos :wk "(a)propos")
	    "h c" '(describe-command :wk "Describe Command")
	    "h f" '(describe-function :wk "Describe Function")
	    "h v" '(describe-variable :wk "Describe Variable")
	    "h k" '(describe-key :wk "Describe Key")
	    "h s" '(describe-symbol :wk "Describe Symbol")
	    "h i" '(info-display-manual :wk "Display Info manual")
	    "h m" '(info-emacs-manual :wk "emacs manual")
	    "h q" '(help-quick-toggle :wk "quick help menu")
	    "h o" '(org-info :wk "Org manual"))
	;; format: on
)
;; === Orgmode ======================================================================================
(use-package org
    :hook (org-mode . visual-line-mode)
    :custom
	(org-directory "~/Notes/")
	(org-agenda-files (list org-directory))
	(org-refile-targets '((org-agenda-files :maxlevel . 5)))
	(org-refile-use-outline-path t)
	(org-outline-path-complete-in-steps nil)
	(org-startup-indented t)
	(org-hide-emphasis-markers t)
	(org-pretty-entities t)
	(org-todo-keywords
	 '((sequence "TODO(t!)" "SOMEDAY(s!)" "WAITING(w@)" "|" "DONE(d!)" "MOVED(m@)" "CANCELLED(c@)")))
	(org-babel-load-languages '((lua . t) (python . t) (shell . t) (emacs-lisp . t)))	
)

(use-package org-modern
    :hook (org-mode . global-org-modern-mode)
    :custom (org-modern-hide-stars " ")
)
(use-package org-appear
    :hook
	(org-mode . org-appear-mode)
	(org-mode . (lambda ()
	    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
	    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))
    :custom
	(org-appear-trigger 'manual)
	(org-appear-autolinks t)
	(org-appear-autosubmarkers t)
	(org-appear-autoentities)
	(org-appear-autokeywords)
	(org-appear-inside-latex)
)
;; === Languages ====================================================================================
(use-package emacs
  :ensure nil
)

;; Auto install and use all tree-sitter grammars
;; Run =treesit-auto-install-all= to install the grammars
(use-package treesit-auto
  :config (global-treesit-auto-mode))

(use-package eglot
    :defer t
    ;; Add your programming modes here to automatically start Eglot,
    ;; assuming you have the respective LSP server installed.
    ;; e.g. rust-analyzer to use Eglot with `rust-mode'.
    :hook (
	((go-mode go-ts-mode) . eglot-ensure)
	(terraform-ts-mode . eglot-ensure)
	(yaml-ts-mode . eglot-ensure)
	(python-ts-mode . eglot-ensure)
))
;; Markdown
(use-package markdown-mode
    ;; These extra modes help clean up the Markdown editing experience.
    ;; `visual-line-mode' turns on word wrap and helps editing commands
    ;; work with paragraphs of text. `flyspell-mode' turns on an
    ;; automatic spell checker.
    :hook
	((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode))
    :init
	(setq markdown-command "multimarkdown")
 )
(use-package python
    :after eglot
)
(use-package terraform-ts-mode
    :vc (:fetcher github :repo kgrotel/terraform-ts-mode)
    :after eglot
    :config
	(add-to-list 'eglot-server-programs '(terraform-ts-mode . ("terraform-ls" "serve")))
)
(use-package go
    :after eglot
    :bind
	(:map go-mode-map ("C-c C-f" . 'gofmt))
    :hook
	(before-save . gofmt-before-save)
    :config (setq tab-width 4)
)

(use-package fish-mode)

;; === File Management ==============================================================================
(use-package treemacs
    :config (treemacs-project-follow-mode) (treemacs-follow-mode)
    :defer t)
(use-package treemacs-evil
    :after (treemacs evil)
    :defer t)
(use-package treemacs-magit
    :after (treemacs magit)
    :defer t)
(use-package dirvish
    :config (dirvish-override-dired-mode)
    :defer t)
    
;; === Shells =======================================================================================
(use-package vterm
    :config (setq vterm-copy-exclude-prompt t)
    :if (not (eq system-type 'windows-nt))
    :bind
        ("C-`" . vterm-toggle)
    )
(use-package vterm-toggle
    :after vterm
    :config
	(setq vterm-toggle-hide-method 'reset-window-configration)
	(setq vterm-toggle-fullscreen-p nil)
	(add-to-list 'display-buffer-alist
	    '((lambda (buffer-or-name _)
		(let ((buffer (get-buffer buffer-or-name)))
		    (with-current-buffer buffer
		    (or (equal major-mode 'vterm-mode)
			(string-prefix-p
			vterm-buffer-name (buffer-name buffer))))))
		(display-buffer-reuse-window display-buffer-at-bottom)
		;;(display-buffer-reuse-window display-buffer-in-direction)
		;;display-buffer-in-direction/direction/dedicated is added in emacs27
		;;(direction . bottom)
		;;(dedicated . t) ;dedicated is supported in emacs27
		(reusable-frames . visible) (window-height . 0.3))))
;; === Customize Stuff ==============================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fish-mode treesit-auto visual-fill-column yaml-mode which-key vterm-toggle vertico verilog-mode vc-use-package use-package-ensure-system-package treemacs-magit treemacs-evil tree-sitter-langs tramp toc-org terraform-ts-mode terraform-mode tabspaces standard-themes solarized-theme rainbow-delimiters paredit page-break-lines org-modern org-appear orderless modus-themes mini-echo markdown-mode marginalia lua-mode lambda-themes kanagawa-theme indent-bars highlight-indent-guides go-mode go general evil-collection elisp-autofmt ef-themes doom-modeline dirvish dashboard corfu consult breadcrumb auto-dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
