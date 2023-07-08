;; Package Management - Elpaca
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)


;; My Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lr/toggle-line-numbering ()
  "Cycle line style."
  (interactive)
  (cond
   ((eq display-line-numbers nil) (setq display-line-numbers t))
   ((eq display-line-numbers t) (setq display-line-numbers 'relative))
   ((eq display-line-numbers 'relative) (setq display-line-numbers nil))))

;; Customizations - Built-ins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-font "Hack Nerd Font 12" nil t)

;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-safe-themes t)

(use-package modus-themes)
(use-package ef-themes)
(use-package standard-themes)
(use-package solarized-theme
  :init
  (setq solarized-use-more-italic t)
  :config
  (load-theme 'solarized-dark :no-confirm))


;; Mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package telephone-line
;;    :init (telephone-line-mode 1))

(use-package doom-modeline
    :custom
    (setq doom-modeline-hud t
	doom-modeline-height 50
	doom-modeline-icon t
	doom-modeline-major-mode-icon nil
	doom-modeline-buffer-encoding nil
	doom-modeline-percent-position '(-3 "")
	doom-modeline-window-width-limit nil
	doom-modeline-vcs-max-length 50)
    (custom-set-faces
    '(mode-line-active ((t (:family "Hack Nerd Font Propo"))))
    '(mode-line-inactive ((t (:family "Hack Nerd Font Propo")))))
  :init (doom-modeline-mode 1)
)

(use-package nerd-icons)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
  (setq dashboard-startup-banner 'official
	dashboard-center-content t
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-footer nil
	)



;; Modal editing - Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC" ;; access leader in insert mode
    )

    (leader-def
    "b" '(:ignore t :wk "Buffers")
	"b b" '(switch-to-buffer :wk "Switch buffer")
	"b d" '(kill-this-buffer :wk "Kill this buffer")
	"b n" '(next-buffer :wk "Next buffer")
	"b p" '(previous-buffer :wk "Previous buffer")

    "w" '(:ignore t :wk "Windows")

    "o" '(:ignore t :wk "Open")

    "t" '(:ignore t :wk "Toggle")
	"t m" '(toggle-menu-bar-mode-from-frame :wk "Menu bar")
	"t f" '(toggle-frame-fullscreen :wk "Fullscreen")
	"t l" '(lr/toggle-line-numbering :wk "Line numbers")

    "h" '(:ignore t :wk "(h)elp")
	"h a" '(apropos :wk "(a)propos")
	"h f" '(describe-function :wk "(f)unction")
	"h v" '(describe-variable :wk "(v)ariable")
	"h m" '(info-emacs-manual :wk "(m)anual emacs")
    ))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-allow-evil-operators t
        which-key-idle-delay 0.7)
)
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dirvish
  :config
  (dirvish-override-dired-mode))

;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package org-modern
  :init (add-hook 'org-mode-hook 'global-org-modern-mode)
)
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package corfu
  :custom
  (corfu-auto t)  
  :init
  (global-corfu-mode)
)

(use-package projectile
  :init (projectile-mode +1))
(use-package page-break-lines
  :init (global-page-break-lines-mode))
