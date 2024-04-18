;;; environment-interop.el --- Diplomacy for Emacs ;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   Configuration for interoperation with things around and outside of Emacs,
;;;   such as the shell and environment variables.
;;; Code:
;;; 

;; Ensure Emacs is willing to act as a server, if it’s not doing so already.
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; If `eud' is installed, let this determine where Emacs server socket files
;; live and set `server-socket-dir' to this; otherwise do nothing here.
(when (executable-find "eud")
  (setq server-socket-dir
	(shell-command-to-string "eud server-socket-dir-path")))


;; Grab the Emacs environment name, if one is set
(eval-and-compile
  (defconst emacs-environment-name (getenv "EMACS_ENV_NAME")))


;; Set preferred shell in `shell-file-name' (for me, it's fish 🐟).
;; This typically falls back to a login shell (bash etc.) otherwise.
(eval-and-compile
  (let ((fish-path (string-trim-right (shell-command-to-string "which fish"))))
      (when (file-exists-p fish-path)
        (setq shell-file-name fish-path))))

;; Location of `emacs-vterm', including dynamic loading library
;; we compile this as a Nix derivation and export the result path to an env variable
(eval-and-compile
  (defconst vterm-load-path (getenv "EMACS_VTERM_PATH")))

;; Get environment variables as per the shell.
;; See https://github.com/d12frosted/homebrew-emacs-plus#injected-path for more.
(use-package exec-path-from-shell
  :straight t
  :init
  ;; list all the environment variables to carry over to here
  (setq exec-path-from-shell-variables
        '("HOMEBREW_PREFIX"
          "INFOPATH"
          "LANG"
          "LSCOLORS"
          "MANPATH"
          "SSH_AGENT_PID"
          "SSH_AUTH_SOCK"
	  "PATH"))
  :config
  (exec-path-from-shell-initialize))


;;; Share `ssh-agent' information with Emacs
;;  (also `gpg-agent', but we're not there yet)
(use-package keychain-environment
  :config
  (keychain-refresh-environment))


;; Remote control & TRAMP
(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        tramp-default-remote-shell "/bin/bash"
        tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/bash")
          (shell-command-switch "-c")))
  
  (connection-local-set-profile-variables
   'tramp-connection-local-default-shell-profile
   '((shell-file-name . "/bin/bash")
     (shell-command-switch . "-c"))))

;; PDF Tools
(eval-and-compile
  (defconst pdf-tools-base-path (getenv "EMACS_PDF_TOOLS"))
  (defconst pdf-tools-site-list-dir
    (concat pdf-tools-base-path "/share/emacs/site-lisp/elpa/pdf-tools-20240317.848/")))

(use-package pdf-tools
  :straight t
  :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq pdf-info-epdfinfo-program
	(expand-file-name "epdfinfo" pdf-tools-site-list-dir))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))

(provide 'environment-interop)
;;; environment-interop.el ends here.
