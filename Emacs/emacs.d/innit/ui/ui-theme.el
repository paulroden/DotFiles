;;; ui-theme.el --- The Theme  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

;;; Tao Theme (https://github.com/11111000000/tao-theme-emacs) because
;;   五色令人目盲；
;;   五音令人耳聾；
;;   五味令人口爽；
;;   馳騁田獵，令人心發狂；
;;   難得之貨，令人行妨。
;;   是以聖人為腹不為目，故去彼取此。
;;             -- 孔子 - 道德經
(use-package tao-theme
  :straight t
  :custom
  (tao-theme-use-sepia t)
  (tao-theme-sepia-saturation 1.03)
  (tao-theme-sepia-depth 7)
  (tao-theme-use-boxes nil)
  :init
  (load-theme 'tao-yang t))


(provide 'ui-theme)
;;; ui-theme.el ends here.
