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

;; Grab the Emacs environment name, if one is set
(eval-and-compile
  (defconst emacs-environment-name (getenv "EMACS_ENV_NAME")))


;; Set preferred shell in `shell-file-name' (for me, it's fish 🐟).
;; This typically falls back to a login shell (bash etc.) otherwise.
(eval-and-compile
  (let ((fish-path (string-trim-right (shell-command-to-string "which fish"))))
      (when (file-exists-p fish-path)
        (setq shell-file-name fish-path))))


;; If `eud' is installed, let this determine where Emacs server socket files
;; live and set `server-socket-dir' to this; otherwise do nothing here.
(when (executable-find "eud")
  (defconst server-socket-dir
	(shell-command-to-string "eud server-socket-dir-path")))


;; Location of `emacs-vterm', including dynamic loading library
;; we compile this as a Nix derivation and export the result path to an env variable
(eval-and-compile
  (defconst vterm-load-path (getenv "EMACS_VTERM_PATH")))

;; Dirvish, which spins on top of dired
(use-package dirvish
  :straight t
  :demand t
  :bind
  (("C-c f" . dirvish-fd)
   ("M-s-l" . dirvish-side)
   ;; ("" . dirvish-dwim)  ;; TODO: set this also?
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)
   ;; mouse bindings
   ("<mouse-1>" . dirvish-subtree-toggle-or-open)
   ("<mouse-2>" . dired-mouse-find-file-other-window)
   ("<mouse-3>" . dired-mouse-find-file))
  :custom
  ;; Don't worry, Dirvish is still performant even if you enable all these attributes
  (dirvish-attributes
   '(file-time
     file-size
     vc-state
     subtree-state
     collapse))
  ;; hide parent directory
  (dirvish-default-layout '(0 0.4 0.6))
  ;; home,
  (dirvish-path-separators '("  ~" "  ⊥" " / "))
  ;; move to trash, rather than destroy
  (delete-by-moving-to-trash t)
  ;; drag & drop now possible
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  ;; easy access to common places
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"              "Home")
     ("d" "~/Downloads/"    "Downloads")
     ("p" "~/Projects/"     "Projects")
     ("t" "~/.Trash/"       "Filth")))
  :config
  ;; Use `eza' instead of `ls' for listing directory previews
  (dirvish-define-preview eza (file)
     "Use eza for directory previews."
     :require ("eza")
     (when (file-directory-p file)
       `(shell . ("eza" "-lag" "--colour=never" ,file))))
  (add-to-list 'dirvish-preview-dispatchers 'eza)
  ;; setup dirvish instead of dired and enable sidebar + peek
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode)
  (dirvish-peek-mode))


;;; It's a secret, ssshutup..!
;;
;;; Share `ssh-agent' information with Emacs
;;  (also `gpg-agent', but we're not there yet)
(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Actually good:
(use-package age
  :straight (age :type git :host github :repo "anticomputer/age.el")
  :demand t
  :custom
  (age-program "rage")
  (age-default-identity "~/.ssh/id_ed25519")
  (age-default-recipient
   '("~/.ssh/id_ed25519.pub"
     "~/.ssh/age_recovery.pub"))
  :config
  (age-file-enable))

(use-package auth-sources
  :straight (:type built-in)
  :after age
  :custom
  (auth-sources
	'((:source "~/.authinfo.age"))))

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

(use-package gptel
  :straight (:type git :host github :repo "karthink/gptel")
  :config
  (defun get-api-key (host)
    (plist-get (car (auth-source-search :host (regxep-quote host))) :secret))
  ;; default: OpenAPI ChatGPT
  (setq gptel-api-key (get-api-key "api.openai.com"))
  ;; Groq offers an OpenAI compatible API
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (get-api-key "api.groq.com")
    :models '("mixtral-8x7b-32768"
              "gemma-7b-it"
              "llama2-70b-4096")))

(provide 'environment-interop)
;;; environment-interop.el ends here.
