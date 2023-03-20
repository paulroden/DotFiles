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


;; Grab the Emacs environment name, if one is set
(eval-and-compile
  (defconst emacs-environment-name (getenv "EMACS_ENV_NAME")))


;; Set preferred shell in `shell-file-name' (for me, it's fish 🐟).
;; This typically falls back to a login shell (bash etc.) otherwise.
(eval-and-compile
  (let ((fish-path (string-trim-right (shell-command-to-string "which fish"))))
      (when (file-exists-p fish-path)
        (setq shell-file-name fish-path))))


;; Get environment variables as per the shell.
;; See https://github.com/d12frosted/homebrew-emacs-plus#injected-path for more.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :init
  ;; list all the environment variables to carry over to here
  (setq exec-path-from-shell-variables
        '("PATH"
          "MANPATH"
          "INFOPATH"
          "LANG"
          "LSCOLORS"
          "HOMEBREW_PREFIX"
          "PYENV_ROOT"
          "SSH_AUTH_SOCK"
          "SSH_AGENT_PID"))
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

(provide 'environment-interop)
;;; environment-interop.el ends here.
