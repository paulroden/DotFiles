;;; ui-modeline.el --- How the Modeline is Displayed  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

;; Modeline Styling
;; (custom-set-faces
;;  `(mode-line
;;    ((t (:family "SF Mono" :size 120 :box (:line-width 3)))))
;;  `(mode-line-inactive
;;    ((t (:family "SF Mono" :size 120 :box (:line-width 3)))))
;;  `(mode-line-highlight
;;    ((t (:family "SF Mono" :size 120 :box (:line-width 3))))))


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
  :custom-face
  (lambda-line-active-status-RW ((t (:foreground "#2f9961"))))
  (lambda-line-active-status-RO ((t (:foreground "#f7dc66"))))
  (lambda-line-active-status-MD ((t (:foreground "#f1184c"))))
  ;; (lambda-line-prefix nil)
  ;; 
  ;; (lambda-line-git-diff-mode-line t)
  ;; (lambda-line-gui-rw-symbol  " ◯ ")
  ;; (lambda-line-gui-mod-symbol " ⊙ ")
  ;; (lambda-line-gui-ro-symbol  " ● ")
  :init (lambda-line-mode))  ;; why init works here, but not config, I am not sure.
  ;; :custom
  ;; (lambda-line-position 'bottom) ;; Set position of status-line
  ;; ;; (lambda-line-abbrev t) ;; abbreviate major modes
  ;; ;; (lambda-line-hspace "  ")  ;; add some cushion
  ;; (lambda-line-prefix t) ;; use a prefix symbol
  ;; ;; (lambda-line-prefix-padding nil) ;; no extra space for prefix
  ;; ;; (lambda-line-status-invert nil)  ;; no invert colors
  ;; ;; (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  ;; ;; (lambda-line-gui-mod-symbol " ⬤")
  ;; ;; (lambda-line-gui-rw-symbol  " ◯")

  ;; (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  ;; :config
  ;; ;; activate lambda-line
  ;; (progn
  ;;   (message "lambda-line-mode available")
  ;;   (lambda-line-mode)))

;; ;; (progn
;; ;;   (message "lambda-line-mode available")
;; ;;   (lambda-line-mode))

;; ;; ;; Show current column number
;; ;; (column-number-mode +1)

(provide 'ui-modeline)
;;; ui-modeline.el ends here.
