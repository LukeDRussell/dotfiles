;;; pre-init.el --- Pre Init file -*- no-byte-compile: t; lexical-binding: t; -*-

(when (eq system-type 'windows-nt)
  (let ((msys-bin "C:/msys64/ucrt64/bin")
        (msys-usr-bin "C:/msys64/usr/bin"))
    (add-to-list 'exec-path msys-usr-bin)
    (add-to-list 'exec-path msys-bin)
    (setenv "PATH" (concat msys-bin ";" msys-usr-bin ";" (getenv "PATH"))))
  ;; That puts MSYS2's gpg first, but it can't read Windows paths, so package
  ;; signatures fail to verify. Use the winget GnuPG instead. Not setopt or
  ;; use-package :custom: `epg-find-configuration' only honours a value marked
  ;; customized (which `customize-set-variable' does), else it searches exec-path.
  (customize-set-variable 'epg-gpg-program "C:/Program Files/GnuPG/bin/gpg.exe"))

;; What init.el would do with `minimal-emacs-package-initialize-and-refresh',
;; minus `package-refresh-contents' (see pre-early-init.el).
(package-initialize)
(require 'use-package)
