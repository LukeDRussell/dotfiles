;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

; Help in info-display-manual --> use-package --> index

(require 'package)
;; Always install packages listed
(setq use-package-always-ensure t)
;; Add MELPA, to the list of accepted package registries.
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)
;; This package adds a new :vc keywords to use-package declarations, with which you can install packages.
;; Note: was merged into emacs 2023-05-16. Might be in emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)


;;;;;;;;;;;;;;;;;;
;; My Functions ;;
;;;;;;;;;;;;;;;;;;

(defun lr/cycle-line-number-style ()
  "Cycles through the line styles I like."
  (interactive)
  (cond
   ((eq display-line-numbers nil)
    (setq display-line-numbers t))
   ((eq display-line-numbers t)
    (setq display-line-numbers 'relative))
   ((eq display-line-numbers 'relative)
    (setq display-line-numbers nil))))
(defun lr/default-line-number-style ()
  (setq display-line-numbers t))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Emacs Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook 'hs-minor-mode) ;; Enable code folding with inbuilt hs
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Display line numbers when in programming modes

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(savehist-mode t)
(recentf-mode t) ;; Keep track of open files
(global-auto-revert-mode t) ;; Keep files up-to-date when they change outside Emacs
(pixel-scroll-precision-mode)

(setq
 window-resize-pixelwise t
 frame-resize-pixelwise t
 load-prefer-newer t
 backup-by-copying t
 backup-directory-alist ;; Backups are placed into your Emacs directory, e.g. ~/.config/emacs/backups
 `(("." . ,(concat user-emacs-directory "backups")))
 visible-bell t
 ring-bell-function 'ignore
 vc-follow-symlinks t
 inhibit-startup-screen t
 insert-directory-program "gls"
 global-auto-revert-non-file-buffers t
 ;; Keep dired up-to-date with files on disk
 scroll-conservatively 101
 ;; When scrolling top or bottom of window, don't recenter point
 scroll-margin 5
 create-lockfiles nil
 native-comp-async-report-warnings-errors "silent"
 ;; Don't lock files. Causes my keyboard to restart
 recentf-exclude ;; Ignore these regex paths from recent file list
 '("/opt/homebrew"
   "/usr/share/emacs/"
   "~/.config/emacs/elpa/"
   (recentf-expand-file-name "~/.config/emacs/elpa")
   (recentf-expand-file-name
    "~/.config/emacs/.cache/treemacs-persist-at-last-error")
   "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"))


;;;;;;;;;;;;;
;; Visuals ;;
;;;;;;;;;;;;;

(tool-bar-mode 0)
(scroll-bar-mode 0)
(electric-pair-mode t)
(show-paren-mode 1)
(if (eq system-type 'darwin)
    (menu-bar-mode 1)
  (menu-bar-mode 0))

;; Themes
(use-package standard-themes)
(use-package ef-themes)
(use-package modus-themes)
(use-package kanagawa-theme)
(use-package lambda-themes
   :vc (:fetcher github :repo lambda-emacs/lambda-themes)
   :custom
   (lambda-themes-set-italic-comments t)
   (lambda-themes-set-italic-keywords t)
   (lambda-themes-set-variable-pitch t) )

 (use-package auto-dark
   :config (auto-dark-mode t)
   :custom
   (auto-dark-dark-theme 'lambda-light-faded)
   (auto-dark-light-theme 'lambda-dark-faded))

;; Font. Load after theme settings

;; (cond
; ;  ((find-font (font-spec :name "Hack Nerd Font Mono"))
;;   (set-face-attribute 'default nil
;;                       :font "Hack Nerd Font Mono"
;;                       :height 150)))
(set-face-attribute 'default nil :height 140)

;; Indent Bars
(use-package indent-bars
  :vc (:fetcher github :repo jdtsmith/indent-bars)
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode)
  :config
  ;; Emacs Plus Plus on MacOS and Windows doesn't support 'Stipples'
  ;; TODO: Enable on Linux
  '(indent-bars-prefer-character t)
  :custom
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.5)
  (indent-bars-pad-frac 0.25)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4)))
 
;; Colour code brackets, braces, parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) 

(use-package breadcrumb
  ;; :init (breadcrumb-mode)
  )

(use-package dashboard
  :config
    (dashboard-setup-startup-hook)
  :custom
    (dashboard-startup-banner 'logo)
    (dashboard-banner-logo-title nil)
    (dashboard-center-content t)
    (dashboard-icon-type 'nerd-icons)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-set-footer nil)
    (dashboard-projects-backend 'project-el)
    (dashboard-display-icons-p t)
    (dashboard-items '((recents . 5) (projects . 5))))

(use-package mini-echo
  :config
    (mini-echo-mode)
  :custom
    (mini-echo-define-segments (
      :long ("major-mode" "buffer-name" "vcs" "buffer-position" "flymake" "process" "selection-info" "narrow" "macro" "profiler" "repeat")
      :short ("buffer-name-short" "buffer-position" "process" "profiler" "selection-info" "narrow" "macro" "repeat")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tabspaces
  :vc (tabspaces :url "https://github.com/mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode)
  )

;;;;;;;;;;;;;;;;;;;;;
;; Editing Support ;;
;;;;;;;;;;;;;;;;;;;;;

;; Discover keybinds with popups
(use-package which-key
  :after evil
  :init (which-key-mode)
  :custom
    (which-key-max-display-columns 5)
    (which-key-add-column-padding 10)
)

;; Minibuffer completion UI
(use-package vertico
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :init (vertico-mode))

;; Minibuffer includes docstrings in margin
(use-package marginalia
  :after vertico :init (marginalia-mode))

;; Adds intellisense-style completion popups
(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
 :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary)
 )

;; Stop caring about the order of search terms in minibuffer filtering
(use-package orderless
  :custom (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

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

;; Evil mode ;;
;;;;;;;;;;;;;;;
(use-package evil
 :init
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 :config
 (evil-mode 1)
 (evil-set-undo-system 'undo-redo)
 ;; Emacs 28+ has this built in
 :custom
 (evil-split-window-below t)
 (evil-vsplit-window-right t)
)

(use-package evil-collection
  :after evil
  :config (evil-collection-init))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control Systems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit)


;;;;;;;;;;;;;;
;; Keybinds ;;
;;;;;;;;;;;;;;
(use-package general
  :config (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer
    lr/leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"	   ;; set leader
    :global-prefix "M-SPC" ;; access leader in insert mode
    )
  ;; format: off
  (lr/leader-def ;; Leader sequences

    "b"   '(:ignore t :wk "buffers")
    "b s" '(switch-to-buffer :wk "switch to named buffer")
    "b m" '(consult-buffer :wk "menu for buffers")
    "b d" '(kill-buffer-and-window :wk "delete buffer")
    "b i" '(ibuffer :wk "IBuffer")
    "b n" '(next-buffer :wk "next buffer")
    "b p" '(previous-buffer :wk "previous buffer")
    "b P" '(consult-project-buffer :wk Project buffers)

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
    
    "e"     '(:ignore t :wk "emacs")
    "e c"   '(:ignore t :wk "config")
    "e c l" '((lambda () (interactive) (load-file user-init-file)) :wk "reload user config")
    "e c o" '((lambda () (interactive) (find-file user-init-file)) :wk "open user config")
    "e o"   '(describe-variable 'system-configuration-options) :wk "emacs build options"
    "e p"   '(list-packages)

    "o"   '(:ignore t :wk "open")
    "o v" '(vterm-toggle :wk "vterm")
    "o d" '(dirvish :wk "open dirvish")
    "o s" '(dirvish-side :wk "open dirvish to side")
    "o D" '(dashboard-open :wk "dashboard")
    "o t" '(treemacs :wk "treemacs")
    "o m" '(magit :wk "magit")
    "o f" '(consult-find :wk "file")
    "o a" '(org-agenda :wk "agenda")

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

;;;;;;;;;;;;;
;; Orgmode ;;
;;;;;;;;;;;;;

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
  '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(use-package org-modern
  :hook (org-mode . global-org-modern-mode)
  :custom (org-modern-hide-stars " ")
  )

(use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    (org-mode . (lambda ()
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
    (org-appear-autolinks t)
    (org-appear-autosubmarkers t)
    (org-appear-autoentities)
    (org-appear-autokeywords)
    (org-appear-inside-latex)
    )

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

;; Emacs Lisp
(add-hook 'emacs-lisp-mode 'lr/default-line-number-style)

;; Get a nice pre-packaged grammar bundle
(use-package tree-sitter-langs)

(use-package eglot
 ;; Add your programming modes here to automatically start Eglot,
 ;; assuming you have the respective LSP server installed.
 ;; e.g. rust-analyzer to use Eglot with `rust-mode'.
 :hook
 ((go-ts-mode . eglot-ensure)
  (terraform-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  )
 )

;; Use Tree-sitter modes instead of 'legacy' modes, for languages
;; that have ts modes
(setq major-mode-remap-alist
      '(
	(bash-mode . bash-ts-mode)
	(c++-mode . c++-ts-mode)
	(c-or-c++-mode . c-or-c++-mode)
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
	;; (yaml-mode . yaml-ts-mode) ; Error: Warning (treesit): Cannot activate tree-sitter, because language grammar for yaml is unavailable (not-found
	(terraform-mode . terraform-ts-mode)
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
 :hook ((markdown-mode . visual-line-mode) (markdown-mode . flyspell-mode))
 :init (setq markdown-command "multimarkdown")
 )

(use-package python
  :after eglot
  )

(use-package terraform-ts-mode
 :vc (:fetcher github :repo kgrotel/terraform-ts-mode)
 :after eglot
 :config
 (add-to-list
  'eglot-server-programs '(terraform-ts-mode . ("terraform-ls" "serve")))
)

(use-package go
 :after eglot
 :bind (:map go-mode-map ("C-c C-f" . 'gofmt))
 :hook
 ;  eglot-ensure
 (before-save . gofmt-before-save)
 :config (setq tab-width 4)
 )

;;;;;;;;;;;;;;;;;;;;;
;; File Management ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
 :config (treemacs-project-follow-mode) (treemacs-follow-mode))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package dirvish
  :config (dirvish-override-dired-mode))
;;;;;;;;;;;;
;; Shells ;;
;;;;;;;;;;;;
(use-package vterm
  :config (setq vterm-copy-exclude-prompt t))
(use-package vterm-toggle
 :after vterm
 :config
 (setq vterm-toggle-hide-method 'reset-window-configration)
 (setq vterm-toggle-fullscreen-p nil)
 (add-to-list
  'display-buffer-alist
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

;;;;;;;;;;;;;;;;;;;;;
;; Customize Stuff ;;
;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "1b623b81f373d49bcf057315fe404b30c500c3b5a387cf86c699d83f2f5763f4" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "7d10494665024176a90895ff7836a8e810d9549a9872c17db8871900add93d5c" "e70e87ad139f94d3ec5fdf782c978450fc2cb714d696e520b176ff797b97b8d2" default))
 '(package-selected-packages
   '(tabspaces tree-sitter-langs org-superstar org-appear yaml-mode which-key vterm-toggle vertico vc-use-package treemacs-magit treemacs-evil toc-org terraform-mode standard-themes rainbow-delimiters paredit page-break-lines org-modern orderless nano-emacs nano modus-themes markdown-mode marginalia lua-mode lambda-themes kanagawa-theme indent-bars highlight-indent-guides helpful golden-ratio go-mode general evil-collection elisp-autofmt ef-themes doom-modeline dirvish denote dashboard corfu consult centaur-tabs breadcrumb auto-dark))
 '(package-vc-selected-packages
   '((tabspaces :url "https://github.com/mclear-tools/tabspaces")
     (:vc-backend Git :url "https://github.com/kgrotel/terraform-ts-mode")
     (indent-bars :vc-backend Git :url "https://github.com/jdtsmith/indent-bars")
     (lambda-themes :vc-backend Git :url "https://github.com/lambda-emacs/lambda-themes"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
