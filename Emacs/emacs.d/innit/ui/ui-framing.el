;;; ui-framing.el --- Frames rule everything around me -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   
;;; Code:

;; Tame the frames
;; Handling the various windows, frames, and popups that Emacs throws around can feel far from a perfect world. There are a number of packages which aim to assist with this: [[https://depp.brause.cc/shackle/][shackle]] and [[https://github.com/karthink/popper][popper]] both aim to group windows depending on usage and enable more sensible organisation; meanwhile [[https://github.com/bmag/emacs-purpose][purpose.el]], [[https://github.com/nex3/perspective-el][perspective-el]] and, perhaps, [[https://github.com/knu/elscreen][ElScreen]] aim more towards configuring workspaces to hold a group of windows together. (Further art includes: ~winner-mode~ – which Popper aims to supplant; [[https://github.com/alphapapa/bufler.el][bufler]], popwin)
;; 💽
;; Popper appeals as the most [[https://www.youtube.com/watch?v=E-xUNlZi3rI][promising]] and tasteful, so we will run with that for now. TODO: Shackle can complement Popper and tell which windows should sit where.
(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  ;; set buffers to be handled by Popper
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*deadgrep.\*"
     "\\*eldoc.*\\*"
     "\\*direnv\\*"
     "\\*xref\\*"
     "\\*Process List\\*"
     help-mode
     occur-mode
     ;; compliation-related
     compilation-mode
     haskell-compilation-mode
     "\\*haskell\\*"
     "\\*cargo-run\\*"
     "\\*rustic-compilation\\*"
     "\\*rustfmt\\*"
     "\\*Go Test\\*"
     "\\*interpretation\\*"
     "\\*Flymake diagnostics\\*"
     "\\*Quail Completions\\*"
     ;; shell modes
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$"  shell-mode
     "^\\*term.*\\*$"   term-mode
     "^\\*vterm.*\\*$"  vterm-mode))
  ;; group by project.el project root, fall back to #'default-directory
  (popper-group-function 'popper-group-by-directory)
  (popper-mode-line '(:eval (propertize " 🫧" 'face 'mode-line-emphasis))) ; cute ;)
  (popper-window-height 22)
  :config
  (popper-mode t)
  (popper-echo-mode t))


;;; Tree 🌳
(use-package treemacs
  :straight t
  :bind
  (:map global-map
        ("s-L"       . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
	("C-x t e"   . treemacs-edit-workspaces))
  :custom
  (treemacs-persist-file "~/DotFiles/Emacs/tree/treemacs-persist")
  (treemacs-width 30)
  (treemacs-no-png-images t)
  (treemacs-collapse-dirs 0)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  :config
  (setq treemacs-fringe-indicator-mode 'only-when-focused)
  :hook
  ;; remove the mode-line from treemacs sidebar
  (treemacs-mode . (lambda () (setq mode-line-format nil)))
  :custom-face
  (treemacs-root-face ((t (:family "SF Mono" :underline nil :weight semi-bold :height 1.0))))
  (treemacs-file-face ((t (:family "SF Mono" :underline nil :weight semi-light :height 1.0))))
  (treemacs-directory-face ((t (:family "SF Mono" :underline nil :weight normal :height 1.0)))))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))


;;; Line highlighting (current line, diffs, etc.)
;; Experimenting with `git-gutter' (v2: https://github.com/syohex/emacs-git-gutter2)
;; Inspired by https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter2
  :straight (:type git :host github :repo "syohex/emacs-git-gutter2")
  :demand t
  :custom
  (git-gutter2-update-interval 0.02)
  (git-gutter2-modified-sign " ")
  (git-gutter2-added-sign " ")
  (git-gutter2-deleted-sign " ")
  :custom-face
  (git-gutter2-modified ((t (:background "#f7dc66"))))
  (git-gutter2-added ((t (:background "#2f9961"))))
  (git-gutter2-deleted ((t (:background "#f1184c"))))
  :config
  (global-git-gutter2-mode +1))

;;; Olivetti for the writer's block
(use-package olivetti
  :straight t
  :commands olivetti-mode
  :custom
  (olivetti-body-width 120)
  (olivetti-minimum-body-width 60)
  (olivetti-style t)
  :custom-face
  (olivetti-fringe ((t (:inherit 'mode-line-inactive))))
  :hook ((org-mode
	  markdown-mode
	  gfm-mode)
	 . olivetti-mode))


;; Line numbers
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 5
              display-line-numbers-widen t)

;; disable line numbers...
(global-display-line-numbers-mode -1)

;;...unless working in most modes
(mapc
 (lambda (mode) (add-hook mode #'display-line-numbers-mode))
 '(agda2-mode
   conf-mode-hook
   haskell-cabal-mode-hook
   makefile-hook
   nix-mode
   org-mode-hook
   prog-mode-hook
   python-mode-hook
   sh-mode-hook
   text-mode-hook))


;;; New Frames Open in the Scratch Buffer
(global-set-key (kbd "s-n")
		#'(lambda ()
		    (interactive)
		    (make-frame)
		    (switch-to-buffer "*scratch*")))

;;; Window Frame Sizing
(global-set-key (kbd "H-f") #'toggle-frame-fullscreen)



(provide 'ui-framing)
;;; ui-framing.el ends here.
