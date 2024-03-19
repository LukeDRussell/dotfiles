;; -*- lexical-binding: t; -*-

;; Package Management

; Help in info-display-manual --> use-package --> index
; use (featurep 'builtin-package-name) to figoure out the name of a builtin module / package / thingie
; e.g. (featurep 'use-package-core) evals to t

(require 'package)

(use-package use-package-core
    :ensure nil
    :init
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    :config
        (package-initialize)
    :custom
	(use-package-always-ensure t) ;; Always install packages listed
	(package-install-upgrade-built-in t)
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

(defun lr/default-line-number-style ()
    (setq display-line-numbers t)
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
        (prog-mode . display-line-numbers-mode) ;; Display line numbers when in programming modes
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
        (auto-dark-light-theme 'kanagawa)
        (auto-dark-dark-theme 'kanagawa)
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


;; === Workspace Management =========================================================================

(use-package tabspaces ;; Each tab is a set of isolated buffers
    :vc (tabspaces :url "https://github.com/mclear-tools/tabspaces")
    :hook (after-init . tabspaces-mode)
)


;; === Editing Support ==============================================================================

(use-package which-key ;; Discover keybinds with popup
    :init (which-key-mode)
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

(use-package meow
    :init
	(defun meow-setup ()
	(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
	(meow-motion-overwrite-define-key
	'("j" . meow-next)
	'("k" . meow-prev)
	'("<escape>" . ignore))
	(meow-leader-define-key
	;; SPC j/k will run the original command in MOTION state.
	'("j" . "H-j")
	'("k" . "H-k")
	;; Use SPC (0-9) for digit arguments.
	'("1" . meow-digit-argument)
	'("2" . meow-digit-argument)
	'("3" . meow-digit-argument)
	'("4" . meow-digit-argument)
	'("5" . meow-digit-argument)
	'("6" . meow-digit-argument)
	'("7" . meow-digit-argument)
	'("8" . meow-digit-argument)
	'("9" . meow-digit-argument)
	'("0" . meow-digit-argument)
	'("/" . meow-keypad-describe-key)
	'("?" . meow-cheatsheet))
	(meow-normal-define-key
	'("0" . meow-expand-0)
	'("9" . meow-expand-9)
	'("8" . meow-expand-8)
	'("7" . meow-expand-7)
	'("6" . meow-expand-6)
	'("5" . meow-expand-5)
	'("4" . meow-expand-4)
	'("3" . meow-expand-3)
	'("2" . meow-expand-2)
	'("1" . meow-expand-1)
	'("-" . negative-argument)
	'(";" . meow-reverse)
	'("," . meow-inner-of-thing)
	'("." . meow-bounds-of-thing)
	'("[" . meow-beginning-of-thing)
	'("]" . meow-end-of-thing)
	'("a" . meow-append)
	'("A" . meow-open-below)
	'("b" . meow-back-word)
	'("B" . meow-back-symbol)
	'("c" . meow-change)
	'("d" . meow-delete)
	'("D" . meow-backward-delete)
	'("e" . meow-next-word)
	'("E" . meow-next-symbol)
	'("f" . mbbeow-find)
	'("g" . meow-cancel-selection)
	'("G" . meow-grab)
	'("h" . meow-left)
	'("H" . meow-left-expand)
	'("i" . meow-insert)
	'("I" . meow-open-above)
	'("j" . meow-next)
	'("J" . meow-next-expand)
	'("k" . meow-prev)
	'("K" . meow-prev-expand)
	'("l" . meow-right)
	'("L" . meow-right-expand)
	'("m" . meow-join)
	'("n" . meow-search)
	'("o" . meow-block)
	'("O" . meow-to-block)
	'("p" . meow-yank)
	'("q" . meow-quit)
	'("Q" . meow-goto-line)
	'("r" . meow-replace)
	'("R" . meow-swap-grab)
	'("s" . meow-kill)
	'("t" . meow-till)
	'("u" . meow-undo)
	'("U" . meow-undo-in-selection)
	'("v" . meow-visit)
	'("w" . meow-mark-word)
	'("W" . meow-mark-symbol)
	'("x" . meow-line)
	'("X" . meow-goto-line)
	'("y" . meow-save)
	'("Y" . meow-sync-grab)
	'("z" . meow-pop-selection)
	'("'" . repeat)
	'("<escape>" . ignore)))
    :config
	(meow-setup)
	(meow-global-mode 1)
)


;; === Version Control Systems ======================================================================

(use-package magit)


;; === Keybinds =====================================================================================


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
)

(use-package org-modern
    :hook (org-mode . global-org-modern-mode)
    :custom (org-modern-hide-stars " ")
)

(use-package org-appear
    :hook
	(org-mode . org-appear-mode)
    :custom
	(org-appear-trigger 'always)
	(org-appear-autolinks t)
	(org-appear-autosubmarkers t)
	(org-appear-autoentities)
	(org-appear-autokeywords)
	(org-appear-inside-latex)
)


;; === Languages ====================================================================================

;; Emacs Lisp
(add-hook 'emacs-lisp-mode 'lr/default-line-number-style)

;; Get a nice pre-packaged grammar bundle
(use-package tree-sitter-langs)

(use-package eglot
    :defer t
    ;; Add your programming modes here to automatically start Eglot,
    ;; assuming you have the respective LSP server installed.
    ;; e.g. rust-analyzer to use Eglot with `rust-mode'.
    :hook (
	(go-ts-mode . eglot-ensure)
	(terraform-ts-mode . eglot-ensure)
	(yaml-ts-mode . eglot-ensure)
	(python-ts-mode . eglot-ensure)
))

;; Use Tree-sitter modes instead of 'legacy' modes, for languages support it
(setq major-mode-remap-alist '(
    (bash-mode . bash-ts-mode)
    (c++-mode . c++-ts-mode)
    (c-or-c++-mode . c-or-c++-ts-mode)
    (c-mode . c-ts-mode)
    (cmake-mode . cmake-ts-mode)
    (csharp-mode . csharp-ts-mode)
    (css-mode . css-ts-mode)
    (go-mode . go-ts-mode)
    (java-mode . java-ts-mode)
    (js2-mode . js-ts-mode)
    (js-json-mode. json-ts-mode)
    (python-mode . python-ts-mode)
    (ruby-mode . ruby-ts-mode)
    (terraform-mode . terraform-ts-mode)
    ;; (yaml-mode . yaml-ts-mode) ; Error: Warning (treesit): Cannot activate tree-sitter, because language grammar for yaml is unavailable (not-found
))

;; Some tree-sitter modes don't exactly match the grammer name
(setq treesit-load-name-override-list
    '((terraform "libtree-sitter-hcl" "tree_sitter_hcl")
    (js "libtree-sitter-js" "tree_sitter_javascript"))
)

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


;; === File Management ==============================================================================

(use-package treemacs
    :config (treemacs-project-follow-mode) (treemacs-follow-mode)
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
 '(custom-safe-themes
   '("e70e87ad139f94d3ec5fdf782c978450fc2cb714d696e520b176ff797b97b8d2" default)))
 '(custom-enabled-themes '(kanagawa))
 '(custom-safe-themes
   '("833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "aed3a896c4ea7cd7603f7a242fe2ab21f1539ab4934347e32b0070a83c9ece01" "a242356ae1aebe9f633974c0c29b10f3e00ec2bc96a61ff2cdad5ffa4264996d" "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "1b623b81f373d49bcf057315fe404b30c500c3b5a387cf86c699d83f2f5763f4" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "7d10494665024176a90895ff7836a8e810d9549a9872c17db8871900add93d5c" "e70e87ad139f94d3ec5fdf782c978450fc2cb714d696e520b176ff797b97b8d2" default))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
