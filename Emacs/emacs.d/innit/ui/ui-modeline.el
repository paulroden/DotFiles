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


;; For a quieter, calmer, more tasteful mode line, we use the
;; [[https://github.com/radian-software/blackout][blackout]] method.
(use-package blackout
  :functions blackout
  :straight t)

(use-package delight
  :straight t)

;; Show current column number
(column-number-mode +1)

(provide 'ui-modeline)
;;; ui-modeline.el ends here.
