;;; init.el --- Luke's emacs config -*- no-byte-compile: t ; lexical-binding: t; -*-

;; === My Functions =================================================================================

(defun my/copy-current-line-position-to-clipboard ()
    "Copy current line in file to clipboard as '</path/to/file>:<line-number>'.
    From https://gist.github.com/kristianhellquist/3082383"
    (interactive)
    (let ((path-with-line-number
           (concat (dired-replace-in-string (getenv "HOME") "~" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
      (kill-new path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard"))))

;; === Package Management ===========================================================================

(use-package use-package
    :ensure nil
    :init
    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)
    :config
    (package-initialize)
    :custom
        (use-package-always-ensure t)
)				      

;; === Emacs core ===================================================================================

(use-package emacs ;; For things without their own /feature/
  :ensure nil
  :hook
  ;; (emacs-startup . toggle-frame-maximized)
  (prog-mode . hs-minor-mode) ;; Enable code folding with inbuilt hs
  (prog-mode . display-line-numbers-mode)
  :bind (
	     ([escape] . keyboard-quit)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)
	     :map minibuffer-local-map
	     ([escape] . minibuffer-keyboard-quit)
	     ([escape] . minibuffer-keyboard-quit)
	     ([escape] . minibuffer-keyboard-quit)
	     ([escape] . minibuffer-keyboard-quit)
	     ([escape] . minibuffer-keyboard-quit)
	     )
  :config
  ;; (tool-bar-mode -1)		 ;; Tool bar is ugly
  ;; (scroll-bar-mode 'right)	 ;; Scroll bar is also ugly
  ;; (menu-bar-mode 1)		 ;; I'm a noob, so keep the menu bar.
  (if (eq system-type 'darwin)
      (setq insert-directory-program "gls"))
  (electric-pair-mode t)
  (show-paren-mode 1)
  (savehist-mode t)           ;; Save minibuffer history
  (recentf-mode t)            ;; Keep track of open files
  (global-auto-revert-mode t) ;; Keep files up-to-date when they change outside Emacs
  (pixel-scroll-precision-mode)
  :custom
  (user-full-name "Luke D Russell")
  (user-mail-address "LukeDRussell+git@outlook.com")
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (load-prefer-newer t)
  (visible-bell t)
  (ring-bell-function 'ignore)
  (display-line-numbers-type 'relative)
  (vc-follow-symlinks t) ;; Stop bugging me when opening my init.el which is a symlink.
  (use-short-answers t)  ;; Stop insisting I type 'yes' when 'y' will do.
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message "Luke") ;; Must match the value of user-login-name
  (initial-scratch-message nil)
  (global-auto-revert-non-file-buffers t) ;; Keep dired up-to-date with files on disk
  (scroll-conservatively 101) ;; When scrolling top or bottom of window, don't recenter point
  (scroll-margin 5)
  (create-lockfiles nil) ;; Don't lock files.
  (make-backup-files nil)
  (save-place-mode 1)
  (native-comp-async-report-warnings-errors "silent")
  (confirm-nonexistent-file-or-buffer nil)
  (native-comp-speed 3)
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
  (custom-safe-themes
   '("fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7"
     default))
  )


;; === Themes ======================================================================================

(use-package ef-themes
  :defer t)

(use-package solarized-theme
  :defer t)

(use-package modus-themes
  :custom
  (modus-themes-common-palette-overrides
   '((bg-line-number-active unspecified)
     (bg-line-number-inactive unspecified))
   ))

(use-package auto-dark
  :init (auto-dark-mode)
  :custom
  (auto-dark-light-theme 'modus-operandi)
  (auto-dark-dark-theme 'modus-vivendi)
  )


;; === Fonts ========================================================================================

(cond
 ((find-font (font-spec :name "Hack Nerd Font Mono"))
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono")))

(set-face-attribute 'default nil :height 120)

;; === Visuals ======================================================================================

;; (use-package indent-bars
;;   :vc (:url "https://github.com/jdtsmith/indent-bars")
;;   :hook
;;   (prog-mode . indent-bars-mode)
;;   :custom
;;   (indent-bars-prefer-character t)
;;   (indent-bars-treesit-support t)
;; )

(use-package dashboard
    :demand t
    :init (dashboard-setup-startup-hook)
    :custom
        (dashboard-startup-banner 'official)
        (dashboard-banner-logo-title nil)
        (dashboard-center-content t)
        (dashboard-vertically-center-content t)
        (dashboard-icon-type 'nerd-icons)
        (dashboard-set-heading-icons t)
        (dashboard-set-file-icons t)
        (dashboard-set-footer nil)
        (dashboard-projects-backend 'project-el)
        (dashboard-display-icons-p t)
        (dashboard-items '((recents . 10) (projects . 10) (bookmarks . 10)))
 ) 
(use-package mini-echo
  :config (mini-echo-mode))

(use-package visual-fill-column
  ;; Centres text when the window is really wide. I don't like turning my head left to read the
  ;; text of ultrawide frames.
  :config
  (global-visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (fill-column 150))


;; === Workspace Management =========================================================================
(use-package tabspaces
  :defer t
  :vc (:url "https://github.com/mclear-tools/tabspaces")
  :custom
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tab-bar-new-tab-choice "*scratch*")
  :commands
  (tabspaces-switch-or-create-workspace
   tabspaces-open-or-create-project-and-workspace)
  )


;; === Editing Support ==============================================================================

(use-package which-key
  :ensure nil
  :config (which-key-mode)
  :custom
  (which-key-max-display-columns 5)
  (which-key-add-column-padding 10)
)

(use-package corfu ;; Intellisense-style completion popups
  :init
    (global-corfu-mode)
    (corfu-popupinfo-mode)
  :custom
    (corfu-auto t)
    (corfu-cycle t)
    (corfu-quit-at-boundary))

(use-package vertico ;; Minibuffer completion UI
  :init (vertico-mode)
  :custom
    (vertico-cycle t)
    (read-buffer-completion-ignore-case t)
    (read-file-name-completion-ignore-case t))

(use-package marginalia ;; Docstrings in minibuffer margin
  :after vertico
  :init (marginalia-mode))

(use-package orderless ;; Stop caring about the order of search terms in minibuffer filtering
  :custom
    (completion-styles '(orderless basic)) (completion-category-overrides
	'((file (styles basic partial-completion)))))

;; (use-package ellama
;;   :bind ("C-c e" . ellama-transient-main-menu)
;;   ;; send last message in chat buffer with C-c C-c
;;   :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
;;   :init (setopt ellama-auto-scroll t)
;;   :config
;;   ;; show ellama context in header line in all buffers
;;   (ellama-context-header-line-global-mode +1))

;; === Modal editing ================================================================================

(use-package evil
  :init
    (setq evil-want-keybinding nil)
  :config
    (evil-mode 1)
  :custom
    (evil-set-undo-system 'undo-redo)
    (evil-split-window-below t)
    (evil-vsplit-window-right t))

(use-package evil-collection
    :after evil
    :config (evil-collection-init))




;; === Version Control Systems ======================================================

(use-package magit
  :defer t)

;; === Org Mode ======================================================================

(use-package org
  :defer t
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :bind (:map org-mode-map ("C-L" . org-store-link))
  :custom
  (org-directory "~/Notes/")
  (org-agenda-files (list org-directory))
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-id-link-to-org-use-id t)
  (org-startup-with-inline-images t)
  (org-pretty-entities t)
  (org-babel-load-languages '((lua . t) (python . t) (shell . t) (emacs-lisp . t)))	
  (org-todo-keywords
   '((sequence "TODO(t!)" "SOMEDAY(s!)" "WAITING(w@)" "|" "DONE(d!)" "MOVED(m@)" "CANCELLED(c@)")))
  )

;; (use-package org-modern
;;   :hook (org-mode . global-org-modern-mode)
;;   :custom (org-modern-hide-stars " ")
;;   )

;; (use-package org-appear
;;   :hook
;;   (org-mode . org-appear-mode)
;;   (org-mode . (lambda ()
;; 		(add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
;; 		(add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))
;;   :custom
;;   (org-appear-trigger 'manual)
;;   (org-appear-autolinks t)
;;   (org-appear-autosubmarkers t)
;;   (org-appear-autoentities)
;;   (org-appear-autokeywords)
;;   (org-appear-inside-latex)
;;   )

;; (use-package org-reverse-datetree
;;   :custom
;;  (org-reverse-datetree-level-formats
;;   '("%Y"											   ; year
;;     (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time)))		   ; month
;;     "%Y W%W"											   ; week
;;     "%Y-%m-%d %A"										   ; date
;;     )) )

;; (use-package htmlize)

;; === Languages ====================================================================================

;; Auto install and use all tree-sitter grammars
;; Run =treesit-auto-install-all= to install the grammars
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package eglot
  :defer t
  :hook
  (python-base-mode . eglot-ensure)
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider))
)

;; (use-package python
;;     :after eglot)

(use-package pet
  :defer t
  :hook (python-base-mode))

(use-package flymake-ruff
  :vc (:url "https://github.com/erickgnavar/flymake-ruff")
  :hook (eglot-managed-mode . flymake-ruff-load))

;; Markdown
(use-package markdown-mode
    ;; These extra modes help clean up the Markdown editing experience.
    ;; `visual-line-mode' turns on word wrap and helps editing commands
    ;; work with paragraphs of text. `flyspell-mode' turns on an
    ;; automatic spell checker.
    :hook
	((markdown-mode . (visual-line-mode flyspell-mode)))
    :init
	(setq markdown-command "multimarkdown")
 )


(use-package fish-mode)

(use-package yaml)

;; Try yaml-pro


;; === File Management ==============================================================================
;; (use-package treemacs
;;     :config (treemacs-project-follow-mode) (treemacs-follow-mode)
;;     :defer t)
;; (use-package treemacs-evil
;;     :after (treemacs evil)
;;     :defer t)
;; (use-package treemacs-magit
;;     :after (treemacs magit)
;;     :defer t)
;; (use-package dirvish
;;     :config (dirvish-override-dired-mode)
;;     :defer t)
   
;; === Shells =======================================================================================

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer t
  :config (exec-path-from-shell-initialize))

;; === Customize Stuff ==============================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(tabspaces))
 '(package-vc-selected-packages
   '((tabspaces :url "https://github.com/mclear-tools/tabspaces") (flymake-ruff :url "https://github.com/erickgnavar/flymake-ruff"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
