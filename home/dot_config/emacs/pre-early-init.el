;;; pre-early-init.el --  -*- no-byte-compile: t; lexical-binding: t; -*-


(setq minimal-emacs-ui-features '(context-menu menu-bar scroll-bar dialogs))

;; Never download package archives at startup. pre-init.el initializes packages
;; instead, and post-init.el has the commands that install and update them.
(setq minimal-emacs-package-initialize-and-refresh nil)

