;;; innit-useful.el --- Common Functions for 'Innit' -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   "Common" in the sense that they are frequently used.
;;;   Load these early and use them often.
;;; Code:

(defconst innit-dir (file-name-directory load-file-name)
  "Path to the directory that contains `innit-environment'.")

;; Innit available on load-path
(add-to-list 'load-path innit-dir)
(add-to-list 'load-path (concat innit-dir "environment"))
(add-to-list 'load-path (concat innit-dir "ui"))
(add-to-list 'load-path (concat innit-dir "typography"))
(add-to-list 'load-path (concat innit-dir "workspace"))

(defun is-macos ()
  "Yields true if invoked on a MacOS machine."
  (eq system-type 'darwin))

(defun is-linux ()
  "Yields true if invoked on a Linux machine."
  (eq system-type 'gnu/linux))
					;
;; Provide useful function to locate `agda-mode' files whenever needed
(defun agda-mode-path ()
  "Provide the location of agda-mode elisp files.
This path is determined using `agda-mode's self-referental `locate' command
which is effecively ensured to work with Nix-based setup TODO: improve"
  (file-name-directory
    (shell-command-to-string "agda-mode locate")))

(provide 'innit-useful)
;;; innit-useful.el ends here.
