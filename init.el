;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; My Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lr/cycle-line-number-style ()
  "Cycles through the line styles I like."
  (interactive)
  (cond
   ((eq display-line-numbers nil) (setq display-line-numbers t))
   ((eq display-line-numbers t) (setq display-line-numbers 'relative))
   ((eq display-line-numbers 'relative) (setq display-line-numbers nil))))

;; Settings

;; Performance tweaks for modern machines.
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; The tool bar is ugly, turn it off.
(tool-bar-mode -1)

;; Set the default font, ensuring it exists.
(cond
  ((find-font (font-spec :name "Hack Nerd Font"))
   (set-face-attribute 'default nil :font "Hack Nerd Font" :height 160)))

;; Add unique buffer names in the minibuffer where there are many identical files.
(require 'uniquify)

;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; Automatically save your place in files
(save-place-mode t)

;; Save history in minibuffer to keep recent commands easily accessible
(savehist-mode t)
;; Ignore these regex paths from recent files
(setq recentf-exclude
    '(
      "/opt/homebrew"
      (recentf-expand-file-name "~/.config/emacs/elpa") 
      (recentf-expand-file-name "~/.config/emacs/.cache/treemacs-persist-at-last-error")
      "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
      )
)


;; Keep track of open files
(recentf-mode t)

;; Keep files up-to-date when they change outside Emacs
(global-auto-revert-mode t)

;; Display line numbers only when in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; The `setq' special form is used for setting variables. Remember
;; that you can look up these variables with "C-h v variable-name".
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; Backups are placed into your Emacs directory, e.g. ~/.config/emacs/backups
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      visible-bell t
      ring-bell-function 'ignore
      display-line-numbers 'relative
      inhibit-startup-screen t
      insert-directory-program "gls"
      ;; Keep dired up-to-date with files on disk
      global-auto-revert-non-file-buffers t
      ;; When scrolling top or bottom of window, don't recenter point
      scroll-conservatively 101
      scroll-margin 5
      )

;; Bring in package utilities so we can install packages from the web.
(require 'package)

;; use-package should assume 'ensure'
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Add MELPA, an unofficial (but well-curated) package registry to the
;; list of accepted package registries. By default Emacs only uses GNU
;; ELPA and NonGNU ELPA, https://elpa.gnu.org/ and
;; https://elpa.nongnu.org/ respectively.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; This package adds a new :vc keywords to use-package declarations, with which you can install packages.
;; Note: was merged into emacs 2023-05-16. Might be in emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; A quick primer on the `use-package' function (refer to
;; "C-h f use-package" for the full details).
;;
;; (use-package my-package-name
;;   :ensure t    ; Ensure my-package is installed
;;   :after foo   ; Load my-package after foo is loaded (seldom used)
;;   :init        ; Run this code before my-package is loaded
;;   :bind        ; Bind these keys to these functions
;;   :custom      ; Set these variables
;;   :config      ; Run this code after my-package is loaded

;; A package with a great selection of themes:
;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
  :config
  (ef-themes-select 'ef-autumn))

;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Adds intellisense-style code completion at point that works great
;; with LSP via Eglot. You'll likely want to configure this one to
;; match your editing preferences, there's no one-size-fits-all
;; solution.
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))

;; Adds LSP support. Note that you must have the respective LSP
;; server installed on your machine to use it with Eglot. e.g.
;; rust-analyzer to use Eglot with `rust-mode'.
(use-package eglot
  ;; Add your programming modes here to automatically start Eglot,
  ;; assuming you have the respective LSP server installed.
  :hook ((go-mode . eglot-ensure)))

;; Evil mode ;;
;;;;;;;;;;;;;;;

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)  ;; Emacs 28+ has this built in
  :custom
   (evil-normal-state-tag " NORMAL ")
   (evil-insert-state-tag " INSERT ")
   (evil-insert-state-message nil)
   (evil-visual-state-tag " VISUAL ")
   (evil-visual-state-message nil) ;; Mode is already displayed in the status bar.
  :custom-face
  (doom-modeline-evil-insert-state ((t (:background "olive drab" :foreground "white smoke"))))
  (doom-modeline-evil-visual-state ((t (:background "medium slate blue" :foreground "white smoke"))))
)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control Systems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit)


;;;;;;;;;;;;;;
;; Keybinds ;;
;;;;;;;;;;;;;;

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer lr/leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"          ;; set leader
    :global-prefix "M-SPC" ;; access leader in insert mode
    )

  (lr/leader-def ;; Leader sequences

;; TODO C-x 8 for emojis
   
   "b" '(:ignore t :wk "buffers")
    "b s" '(switch-to-buffer :wk "switch to named buffer")
    "b m" '(buffer-menu :wk "menu for buffers")
    "b k" '(kill-this-buffer :wk "kill buffer")
    "b n" '(next-buffer :wk "next buffer")
    "b p" '(previous-buffer :wk "previous buffer")
    "b d" '(dashboard-open :wk "dashboard")

    "e" '(:ignore t :wk "emacs")
    "e c" '(:ignore t :wk "config")
    "e c r" '((load-file user-init-file) :wk "(r)eload user config")

    "o" '(:ignore t :wk "open")
    "o v" '(vterm-toggle :wk "vterm")
    "o d" '(dashboard-open :wk "dashboard")
    "o t" '(treemacs :wk "treemacs")

    "q" '(:ignore t :wk "quit")
    "q r" '(restart-emacs :wk "Restart emacs")
    "q n" '(restart-emacs-start-new-emacs :wk "restart to New emacs")
    "q q" '(save-buffers-kill-terminal :wk "Quit emacs")

    "u" '(:ignore t :wk "ui")
    "u m" '(toggle-menu-bar-mode-from-frame :wk "Menu bar")
    "u l" '(lr/cycle-line-number-style :wk "Line numbers")
    "u F" '(toggle-frame-fullscreen :wk "Fullscreen")
    "u T" '(tear-off-window :wk "Tear off window to new fram%:e")

    "g" '(:ignore t :wk "git")
    "g s" '(magit-status :wk "status")

    "h" '(:ignore t :wk "(h)elp")
    "h a" '(apropos :wk "(a)propos")
    "h f" '(describe-function :wk "(f)unction describe")
    "h v" '(describe-variable :wk "(v)ariable describe")
    "h k" '(describe-key :wk "(k)ey describe")
    "h m" '(info-emacs-manual :wk "(m)anual emacs")
    "h q" '(help-quick-toggle :wk "(q)uick menu")
    "h o" '(org-info :wk "(o)rg manual")
;;    "h o" '(info-org)  ;; Org-mode manual
    ))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;;;;;;;;;;;;
;; Visuals ;;
;;;;;;;;;;;;;

;; Example of built in package manager using a git target.
(use-package breadcrumb
  :vc (:fetcher github :repo joaotavora/breadcrumb)
  :init (breadcrumb-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-allow-evil-operators t
        which-key-idle-delay 0.7))

(use-package nerd-icons
  :config
  (cond
    ((find-font (font-spec :name "Hack Nerd Font"))
     (setq nerd-icons-font-family "Hack Nerd Font"))))

(use-package dashboard
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official
	dashboard-center-content t
	dashboard-display-icons-p nil  ;; Currently breaks about missing 'nil' icon in octicon
	dashboard-icon-type 'nerd-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-footer nil
        dashboard-projects-backend 'project-el
        ))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq mode-line-percent-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 30
        doom-modeline-modal-icon nil))


;;;;;;;;;;;;;;
;; Org-mode ;;
;;;;;;;;;;;;;;

;; `org-mode' is great but Denote makes it even better by adding
;; features that you'd find in something like Obsidian (like
;; backlinks!). You can write your notes in org, markdown, or plain
;; text, though I recommend giving `org-mode' a try if you've never
;; used it before. The Denote manual is also excellent:
;; https://protesilaos.com/emacs/denote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package denote                                 ;;
;;   :custom                                           ;;
;;   (denote-known-keywords '("emacs" "journal"))      ;;
;;   ;; This is the directory where your notes live.   ;;
;;   (denote-directory (expand-file-name "~/denote/")) ;;
;;   :bind                                             ;;
;;   (("C-c n n" . denote)                             ;;
;;    ("C-c n f" . denote-open-or-create)              ;;
;;    ("C-c n i" . denote-link)))                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-directory "~/Notes/"
      org-agenda-files (list org-directory)
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-startup-indented t
      org-indent-indentation-per-level 2
      org-hide-emphasis-markers t
      )
)
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package org-modern
  :init (add-hook 'org-mode-hook 'global-org-modern-mode))
(use-package page-break-lines
  :init (global-page-break-lines-mode))


;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

(use-package paredit
;; ELisp
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package go-mode
  :bind (:map go-mode-map
	      ("C-c C-f" . 'gofmt))
  :hook (before-save . gofmt-before-save))

(use-package markdown-mode
  ;; These extra modes help clean up the Markdown editing experience.
  ;; `visual-line-mode' turns on word wrap and helps editing commands
  ;; work with paragraphs of text. `flyspell-mode' turns on an
  ;; automatic spell checker.
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package yaml-mode)


;;;;;;;;;;;;;;;;;;;;;
;; File Management ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs)

(use-package treemacs-evil
:after (treemacs evil))

(use-package treemacs-magit
:after (treemacs magit))

(use-package dirvish
  :after (dirvish-override-dired-mode))


;;;;;;;;;;;;
;; Shells ;;
;;;;;;;;;;;;

(use-package vterm
  :config
  (setq vterm-copy-exclude-prompt t)
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
			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 ;;(display-buffer-reuse-window display-buffer-in-direction)
		 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
		 ;;(direction . bottom)
		 ;;(dedicated . t) ;dedicated is supported in emacs27
		 (reusable-frames . visible)
		 (window-height . 0.3))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org yaml-mode which-key vterm-toggle vertico vc-use-package treemacs-magit treemacs-evil toc-org paredit page-break-lines org-modern markdown-mode marginalia helpful go-mode general evil-collection ef-themes doom-modeline dirvish denote dashboard corfu breadcrumb)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
