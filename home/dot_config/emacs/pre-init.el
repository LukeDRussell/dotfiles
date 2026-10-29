;;; pre-init.el --- Pre Init file -*- no-byte-compile: t; lexical-binding: t; -*-

(when (eq system-type 'windows-nt)
  (let ((msys-bin "C:/msys64/ucrt64/bin")
        (msys-usr-bin "C:/msys64/usr/bin"))
    (add-to-list 'exec-path msys-usr-bin)
    (add-to-list 'exec-path msys-bin)
    (setenv "PATH" (concat msys-bin ";" msys-usr-bin ";" (getenv "PATH")))))
