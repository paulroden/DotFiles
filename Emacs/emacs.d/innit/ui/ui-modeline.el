;;; ui-modeline.el --- How the Modeline is Displayed  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

(use-package all-the-icons
  :straight t
  :demand t)

(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line")
  :requires all-the-icons
  :demand t
  :custom
  (lambda-line-position 'bottom)
  (lambda-line-hspace " ")
  (lambda-line-space-top    +0.30)
  (lambda-line-space-bottom -0.30)
  (lambda-line-status-invert t)
  (lambda-line-gui-rw-symbol  " ◯ ")
  (lambda-line-gui-mod-symbol " ⊙ ")
  (lambda-line-gui-ro-symbol  " ● ")
  (lambda-line-symbol-position -0.05)
  (lambda-line-vc-symbol " @ ")
  :custom-face
  (lambda-line-active-status-RW ((t (:foreground "#2f9961"))))
  (lambda-line-active-status-RO ((t (:foreground "#f1184c"))))
  (lambda-line-active-status-MD ((t (:foreground "#f7dc66"))))
  :config
  ;; Modeline Styling
  (let ((family "SF Mono")
	(weight 'semi-light)
	(size 130))
    (custom-set-faces
     `(mode-line
       ((t (:family ,family :weight ,weight :height ,size :box (:line-width (0 . 1))))))
     `(mode-line-active
       ((t (:family ,family :weight ,weight :height ,size :box (:line-width (0 . 1))))))
     `(mode-line-inactive
       ((t (:family ,family :weight ,weight :height ,size :box (:line-width (0 . 1))))))
     `(mode-line-highlight
       ((t (:family ,family :weight ,weight :height ,size :box (:line-width (0 . 1))))))))
  :init
  (lambda-line-mode))  ;; why init works here, but not config, I am not sure.

;; ;; ;; Show current column number
;; ;; (column-number-mode +1)

(provide 'ui-modeline)
;;; ui-modeline.el ends here.
