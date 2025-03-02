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

(add-hook 'kill-emacs-hook #'recentf-cleanup)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))

(use-package emacs
  :ensure nil
  :hook
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
  (cond
   ((find-font (font-spec :name "Hack Nerd Font Mono"))
    (set-face-attribute 'default nil :font "Hack Nerd Font Mono")))
  (set-face-attribute 'default nil :height 120)
  )

(use-package modus-themes
  :custom
  (modus-themes-common-palette-overrides
   '((bg-line-number-active unspecified)
     (bg-line-number-inactive unspecified))
   ))

(use-package ef-themes)

(use-package auto-dark
  :config (auto-dark-mode)
  :custom
  (auto-dark-light-theme 'ef-autumn)
  (auto-dark-dark-theme 'ef-autumn)
  )

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))
  
(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :requires nerd-icons
  :custom
  (dashboard-startup-banner 'official)
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-footer t)
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-items '((recents . 10) (projects . 10) (bookmarks . 10)))
  )

(use-package eglot
  :hook
  (python-base-mode . eglot-ensure)
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

;; === Customizations ===================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("475251f6f8fb699a88bd5628c5f8d9ef0862a4d8063509938f7d0be0c482d1fc"
     "4d143475f66d03177c5c0c8109954829c3efaa91a2ce304a78d20d9915851f25"
     "5b642c9a3a1700ead57d5569135e912ef40455be7561d77784be4cfdd3795606" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
