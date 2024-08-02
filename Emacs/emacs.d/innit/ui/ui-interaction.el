;;; ui-interaction.el --- The part between you and Emacs  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   Pop-up menu? here.
;;;   Window interaction thing? here.
;;;   Keyboard shorcut? here.
;;; Code:

;;; Input methods
;; The Agda input method is worth having almost everywhere. We're going to
;; eagerly load it so its at our fingertips from the outset.
;; Don't count your quails before the egfs have hatched...
;; Agda's neat input method runs on the quail minor mode (included in Emacs,
;; originally from AIST Japan), we `require' this in advance.
(require 'quail)

(use-package agda-input
  :load-path (lambda () (agda-mode-path))
  :demand t
  :after quail
  :hook ((text-mode prog-mode) . (lambda () (set-input-method "Agda")))
  :custom (default-input-method "Agda"))


;; Help me remember where to plonk my next digit
(use-package which-key
  :straight t
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))


;; Cursors, selections, regions, curses
(use-package expand-region
  :straight t
  :demand t
  :bind (("C--" . er/contract-region)
	 ("C-=" . er/expand-region)))


;; Electric Pairs
(use-package electric
  :bind* (("C-c '" . electric-quote-mode)
	  ("C-c (" . electric-pair-mode))
  :hook ((org-mode . electric-quote-mode)
         ((conf-mode
           prog-mode
           text-mode)
          . electric-pair-mode)))

;; Parens
(use-package highlight-parentheses
  :straight t
  :custom
  (highlight-parentheses-highlight-adjacent t)
  (highlight-parentheses-colors
   (mapcar (lambda (l)
	  (apply
	   #'color-rgb-to-hex (append (color-hsl-to-rgb 0.63 0.75 l) '(2))))
	   (number-sequence 0.42 0.12 -0.05)))
  :hook ((conf-mode
  	 org-mode	
         prog-mode
         text-mode) . highlight-parentheses-mode))

;;; Menus and Navigation
;; Avy navygation
(use-package avy
  :straight t)

;; Avy menu
(use-package avy-menu
  :straight t)

;; Popup menus via ace
;; https://github.com/mrkkrp/ace-popup-menu
;; (this may be a life-saver for Eglot's code-action menu)
(use-package ace-popup-menu
  :straight t
  :demand t
  :config
  (ace-popup-menu-mode 1))

(use-package emacs
  ;; Customisation of Emacs' completion defaults, per Minad's suggestions
  ;; - for Corfu completion (https://github.com/minad/corfu#installation-and-configuration)
  :init
  (setq completion-cycle-threshold 3)
  ;; compatibility with Emacs 28..
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  ;; Enable completion at point with <TAB>
  ;; first <TAB> will indent, subsequent will attempt to autocomplete
  (setq tab-always-indent #'complete)

  ;; - for the minibuffer with Vertico (https://github.com/minad/vertico#configuration)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Emacs 28 and thereafter: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  
  ;; Text interaction
  ;; Delete active/selected text when inserting (yanking) for fewer surprises.
  (delete-selection-mode t)
  
  ;; Enable minibuffers-in-minibuffers-in-minibuffers, and show how deep they go:
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)

  ;; General Keybindings
  (global-set-key (kbd "s-Z") #'undo-redo))

;; Cursor as a bar
(setq-default cursor-type 'bar)

(provide 'ui-interaction)
;;; ui-interaction.el ends here.

