;;; workspace-terminal.el --- Terminal Interaction  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

;;; Setup for `vterm' essentially.
;;; As a built-in terminal emulator in Emacs, the world says that nothing comes close to [[https://github.com/akermu/emacs-libvterm#given-that-eshell-shell-and-ansi-term-are-emacs-built-in-why-should-i-use-vterm][vterm]]. Note that ~cmake~ is required for building, so best to ~brew install cmake~ before first invocation here <TODO: manage these extrinsic requirements, perhaps with something like ~use-package-ensure-system-package~ >.
;;; Most of the below is a ripoff of PatrickT’s setup.
(use-package vterm
  :straight t
  :custom
  ;; don't ask to compile the vterm C module if it's not already compiled
  (vterm-always-compile-module t)
  ;; update state more frequenlty than the default
  (vterm-timer-delay 0.05)
  ;; swim with the fish (see `environment/environment-interop.el' for (re-)definition
  (vterm-shell shell-file-name)
  :config
  (defun local/vterm-for-project ()
    "Invoke `vterm' in the current project root.
  Switch to the project-specific term buffer if it already exists."
    (interactive)
    (let* ((project (project-current))
           (buffer  (format "*vterm %s*" (consult--project-name (project-root project)))))
      (unless (buffer-live-p (get-buffer buffer))
        (unless (require 'vterm nil 'noerror)
          (error "Package `vterm' is not available"))
        (vterm buffer)
        (vterm-send-string (concat "cd " (project-root project)))
        (vterm-send-return))
      (switch-to-buffer buffer))))

(use-package vterm-toggle
  :straight t
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in a another window.")
  (vterm-toggle-scope 'project)
  ;; allow ⇧⌘T to be a global shortcut for showing/hiding vterm
  :bind* ("C-s-t" . #'vterm-toggle)
  :bind (:map vterm-mode-map
              ("s-T" . #'vterm) ;; new tab
              ("s-v" . #'vterm-yank)
              ;; why choose.
              ("C-y" . #'vterm-yank)))


(provide 'workspace-terminal)
;;; workspace-terminal.el ends here.
