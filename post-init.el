;;; post-init.el --- Post Init file -*- no-byte-compile: t; lexical-binding: t; -*-


;; === Load Compile Angel first, so everything else goes faster ================

(use-package compile-angel
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose t)
  :config
  (compile-angel-on-load-mode)
  :hook (emacs-lisp-mode . compile-angel-on-save-local-mode)
  )


;; === My Functions ============================================================

(defun my/copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'.
    From https://gist.github.com/kristianhellquist/3082383"
  (interactive)
  (let ((path-with-line-number
         (concat (dired-replace-in-string (getenv "HOME") "~" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))


;; === Emacs ===================================================================

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode) ;; Enable code folding with inbuilt hs
  (prog-mode . display-line-numbers-mode)
  (kill-emacs . recentf-cleanup)
  (after-init . global-auto-revert-mode)
  (after-init . save-place-mode)
  (after-init . (lambda()
                  (let ((inhibit-message t))
                    (recentf-mode 1))))
  :bind
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  :config
  (electric-pair-mode)
  (if (eq system-type 'darwin)
      (setq insert-directory-program "gls"))
  (set-face-attribute 'default nil :height 120)
  (if (eq system-type 'windows-nt)
      (set-fontset-font t 'symbol "Segoe UI Symbol"))
  (cond
   ((find-font (font-spec :name "Hack Nerd Font Mono"))
    (set-face-attribute 'default nil :font "Hack Nerd Font Mono")))
  :custom
  (user-full-name "Luke D Russell")
  (user-mail-address "LukeDRussell+git@outlook.com")
  (display-line-numbers-type 'relative)
  (scroll-margin 5)
  )

;; === Modal Editing=============================================================

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :custom
  (evil-set-undo-system 'undo-redo)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  )

(use-package evil-collection
  :after evil
  :config (evil-collection-init))


;; === Colour themes ============================================================

(use-package modus-themes
  :custom
  (modus-themes-common-palette-overrides
   '((bg-line-number-active unspecified)
     (bg-line-number-inactive unspecified))
   ))

(use-package ef-themes
  :defer t)

(use-package solarized-theme
  :defer t)

(use-package kanagawa-themes
  :defer t)

(use-package auto-dark
  :config (auto-dark-mode)
  :custom
  (auto-dark-light-theme 'modus-operandi)
  (auto-dark-dark-theme 'modus-vivendi)
  )


;; === Visuals ==================================================================

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))

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

(use-package doom-modeline
  :config (doom-modeline-mode)
  :after evil
  :custom
  (doom-modeline-buffer-file-name truncate-upto-project)
  (doom-modeline-modal-icon nil)
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


;; === Help =====================================================================

(use-package which-key
  :ensure nil
  :config (which-key-mode)
  :custom
  (which-key-max-display-columns 5)
  (which-key-add-column-padding 10)
)

;; === Tooling ==================================================================

(use-package transient)

(use-package magit)

(use-package corfu ;; Intellisense-style completion popups
  :init
    (global-corfu-mode)
    (corfu-popupinfo-mode)
  :custom
    (corfu-auto t)
    (corfu-cycle t)
    (corfu-quit-at-boundary))

(use-package consult)

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


;; === IDE ======================================================================

(use-package eglot
  :hook
  (python-base-mode . eglot-ensure)
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

;; === Org Mode =================================================================

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

(use-package org-modern
  :hook (org-mode . global-org-modern-mode)
  :custom
  (org-modern-hide-stars " ")
)



;; === Customizations ===================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("721ee346f848ecdebc613eab869de3f26e25205d77b5d6e0bca161c6578ba9e6"
     "6ded0a405a69156fcf594300a2c73c7fcfb2746f19edf9d17fc67285e1a0fc72"
     "24746d6cf9fd78c149761816aae6ceb248d8bd990523ce27aae6ec556c3dbda4"
     "c9792eaf9b7270d3331228072362e2f8dcd60a0cd43f678c470b0089e421b702"
     "1ba19ef4cffe927eb8c8108079ef731a4894c6702aa7100541d0dd6c66248d4f"
     "c98e359e2ec6d95e29f006202641f7bafc9e6e204f937d4d518c3ef154ed479b"
     "cba0922a7ed1508d3beeb797bb9932ffecf0bcc0efbeb1be3c5f31de201db407"
     "475251f6f8fb699a88bd5628c5f8d9ef0862a4d8063509938f7d0be0c482d1fc"
     "4d143475f66d03177c5c0c8109954829c3efaa91a2ce304a78d20d9915851f25"
     "5b642c9a3a1700ead57d5569135e912ef40455be7561d77784be4cfdd3795606" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
