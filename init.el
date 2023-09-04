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

;; use-package instructions
;; :init - Always run. Slows down startup.
;; :commands - Enables lazy loading. The specified functions from a package can be called, without actually loading the package.
;;
;; :bind - Shortcut macro, bind a key combo to a command (see above). Can put a description in the binding for which-key
;;     ("C-:" ("Jump to char" . avy-goto-char)
;;             "M-g f" ("Jump to line" . avy-goto-line)))
;;
;; :autoload - used for non-interactive functions.
;; :custom - Personal customizations. Not functionally different to :config. Equivelant to useing emacs' customize-option.
;; :config - Run after loading. Can nest other packages inside this with (use-package ...).


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

(setq
 visible-bell t
 ring-bell-function 'ignore
 inhibit-startup-screen t
)

(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)


;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-font "Hack Nerd Font 14" nil t)

;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ef-themes
  :init
    (ef-themes-select 'ef-autumn)
)

;; Mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (mode-line-percent-position nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-modal-icon nil)
)


(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font")
)

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
	)
)

;; Modal editing - Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq
   evil-normal-state-tag " NORMAL "
   evil-insert-state-tag " INSERT "
   evil-insert-state-message nil
   evil-visual-state-tag " VISUAL "
   evil-visual-state-message nil
   )
(custom-set-faces
 '(doom-modeline-evil-insert-state ((t (:background "olive drab" :foreground "white smoke"))))
 '(doom-modeline-evil-visual-state ((t (:background "medium slate blue" :foreground "white smoke")))))
)

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


;; Try not to replicate vim keybinds, as which-key should help learn them.
    (leader-def
    "b" '(:ignore t :wk "Buffers")
	"b b" '(switch-to-buffer :wk "Switch to named...")
	"b m" '(buffer-menu-other-window :wk "Menu")
	"b d" '(kill-this-buffer :wk "Close this buffer")
	"b n" '(next-buffer :wk "Next buffer")
	"b p" '(previous-buffer :wk "Previous buffer")

    "o" '(:ignore t :wk "Open")
	"o t" '(vterm-toggle :wk "Terminal")

    "q" '(:ignore t :wk "Quit")
	"q r" '(restart-emacs :wk "Restart emacs")
	"q n" '(restart-emacs-start-new-emacs :wk "New emacs")
	"q q" '(save-buffers-kill-terminal :wk "Quit emacs")

    "u" '(:ignore t :wk "User Interface")
	"u m" '(toggle-menu-bar-mode-from-frame :wk "Menu bar")
	"u l" '(lr/toggle-line-numbering :wk "Line numbers")
	"u f" '(treemacs :wk "File sidebar")
	"u F" '(toggle-frame-fullscreen :wk "Fullscreen")

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
        which-key-idle-delay 0.7))


;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory "~/Notes/"
      org-refile-targets org-directory
      org-agenda-files (list org-directory))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package org-modern
  :init (add-hook 'org-mode-hook 'global-org-modern-mode))
(use-package page-break-lines
  :init (global-page-break-lines-mode))

(setq org-startup-indented t
      org-indent-indentation-per-level 2)

;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs)
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-evil
  :after (treemacs evil))

;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :custom
  (corfu-auto t)  
  :init
  (global-corfu-mode))

(use-package restart-emacs)

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
 '(custom-safe-themes
   '("2698856af1babf094cf3779d6eacfb49e1edce39a9421844fcad971c92031fe3" default))
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-evil-insert-state ((t (:background "olive drab" :foreground "white smoke"))))
 '(doom-modeline-evil-visual-state ((t (:background "medium slate blue" :foreground "white smoke")))))
