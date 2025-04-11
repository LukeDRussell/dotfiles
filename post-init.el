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
  (after-init . savehist-mode)
  (after-init . (lambda()
                  (let ((inhibit-message t))
                    (recentf-mode 1))))
  :bind (
	     ([escape] . keyboard-quit)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)
	     :map minibuffer-local-map
	     ([escape] . abort-minibuffers)
	     )

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
  ;; (completion-styles '(basic flex initials))
  )

;; === Colour themes ============================================================

(use-package modus-themes
  :defer t
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
  :defer t
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

(use-package visual-fill-column
  ;; Centres text when the window is really wide. I don't like turning my head left to read the
  ;; text of ultrawide frames.
  :config
  (global-visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (fill-column 150))


;; === Modal Editing=============================================================

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-set-undo-system 'undo-redo)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))
  (which-key-add-key-based-replacements
    "SPC b" "buffers"
    "SPC e" "emacs"
    "SPC h" "help"
    "SPC o" "open"
    "SPC q" "quit"
    )
  (evil-define-key nil 'global
    (kbd "<leader> bn") '("Next buffer" . evil-next-buffer)
    (kbd "<leader> bp") '("Prev buffer" . evil-prev-buffer)
    (kbd "<leader> bs") '("Switch buffer" . switch-to-buffer)
    (kbd "<leader> bk") '("Kill current buffer" . kill-current-buffer)
    (kbd "<leader> bK") '("Kill a buffer" . kill-buffer)
    (kbd "<leader> bP") '("Project switch buffer" . project-switch-to-buffer)

    (kbd "<leader> ha") '("apropos" . apropos)
    (kbd "<leader> h@") '("at point" . helpful-at-point)
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
    (kbd "<leader> hu") '("use-package" . (lambda () (interactive) (info-display-manual "use-package")))
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
    (kbd "<leader> od") '("dired" . dired)
    (kbd "<leader> oD") '("Dashboard". dashboard-open)

    (kbd "<leader> qr") '("restart" . restart-emacs)
    (kbd "<leader> qq") '("quit" . save-buffers-kill-emacs)
    )
  )

;;    "e"   '(:ignore t :wk "emacs")
;;    "e c" '((lambda () (interactive) (find-file user-init-file)) :wk "open user config")
;;    "e o" '((lambda () (interactive) (describe-variable 'system-configuration-options)) :wk "emacs build options")
;;    "e p" '(elpaca-manager :wk "packages")
;;    "e r" '((lambda () (interactive) (find-file )))
;;    "e u" '(package-menu-filter-upgradable :wk "show packages that can be upgraded")
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


;; === Help =====================================================================

(use-package which-key
  :ensure nil
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
  (corfu-quit-at-boundary))

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


;; (use-package ellama
;;   :bind ("C-c e" . ellama-transient-main-menu)
;;   ;; send last message in chat buffer with C-c C-c
;;   :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
;;   :init (setopt ellama-auto-scroll t)
;;   :config
;;   ;; show ellama context in header line in all buffers
;;   (ellama-context-header-line-global-mode +1))


;; === IDE ======================================================================

;; Auto install and use all tree-sitter grammars
;; Run =treesit-auto-install-all= to install the grammars
(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package eglot
  :ensure nil
  :hook
  (python-base-mode . eglot-ensure)
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

(use-package apheleia
  :ensure t
  :defer t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

(use-package yaml
  :hook yaml-pro-mode
  )

(use-package yaml-pro
  :defer t)

;; Markdown
(use-package markdown-mode
  :defer t
  :init (setq markdown-command "multimarkdown")
  )

;; Python
(use-package pet
  :defer t
  :hook (python-base-mode))

;; === Org and Calendar =========================================================

(use-package calendar
  :defer t
  :ensure nil
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

(use-package org-reverse-datetree
  :defer t
  :custom
  ;; Check function =format-time-string= for syntax.
  (org-reverse-datetree-level-formats
   '("%Y"											   ; year
     (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time)))		   ; month
     "%Y W%V"											   ; week
     "%Y-%m-%d %A"										   ; date
     )) )

(use-package htmlize
  :defer t)



;; === Customizations ===================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729" "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077" "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     "721ee346f848ecdebc613eab869de3f26e25205d77b5d6e0bca161c6578ba9e6" "6ded0a405a69156fcf594300a2c73c7fcfb2746f19edf9d17fc67285e1a0fc72"
     "24746d6cf9fd78c149761816aae6ceb248d8bd990523ce27aae6ec556c3dbda4" "c9792eaf9b7270d3331228072362e2f8dcd60a0cd43f678c470b0089e421b702"
     "1ba19ef4cffe927eb8c8108079ef731a4894c6702aa7100541d0dd6c66248d4f" "c98e359e2ec6d95e29f006202641f7bafc9e6e204f937d4d518c3ef154ed479b"
     "cba0922a7ed1508d3beeb797bb9932ffecf0bcc0efbeb1be3c5f31de201db407" "475251f6f8fb699a88bd5628c5f8d9ef0862a4d8063509938f7d0be0c482d1fc"
     "4d143475f66d03177c5c0c8109954829c3efaa91a2ce304a78d20d9915851f25" "5b642c9a3a1700ead57d5569135e912ef40455be7561d77784be4cfdd3795606" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
