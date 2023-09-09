;; Package Sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


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
 display-line-numbers 'relative
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

;; File Browsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs)
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-evil
  :after (treemacs evil))

(use-package dirvish
  :init (dirvish-override-dired-mode))


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

