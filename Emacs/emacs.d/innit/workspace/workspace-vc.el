;;; workspace-vc.el --- Version Control -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   
;;; Code:

;; hack to eliminate some kind of weirdness
(unless (boundp 'bug-reference-auto-setup-functions)
  (defvar bug-reference-auto-setup-functions '()))

;;; Autoreversion -- this is worth enabling as the default
(use-package autorevert
  :blackout (auto-revert-mode . " 􀖊")
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;;; Magit and associated shiny porcelain
(use-package magit
  :straight t
  :config (setq auto-revert-check-vc-info t))

;; <TODO: use-package-ensure-system-package for `libgit'>
(use-package libgit
  :straight t
  :after magit)

(use-package magit-libgit
  :disabled
  :after (magit libgit))

;; Integration with Github and other so-called "forges"
(use-package forge
  :after magit)


;; Use the stylish, modern, Delta to show diff’s
;; (https://dandavison.github.io/delta)
(use-package magit-delta
  :straight t
  :hook (magit-mode . magit-delta-mode))



(provide 'workspace-vc)
;;; workspace-vc.el ends here
