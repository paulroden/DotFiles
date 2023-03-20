;;; environment-lisp.el --- Lisp, and how to interpret it  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

;; Elisp language extension & addtions
;; Common Lisp (note that `cl-lib' is the library to use these days:
;;  https://emacs.stackexchange.com/questions/48109/require-cl-or-require-cl-lib
(require 'cl-lib)

;; Some packages from Magnar Sveen to make Elisp less of a chore.
;; `-' nicer maps and so on
(use-package dash
  :straight t)
;; `s' trings
(use-package s
  :straight t)
;; `f'ile paths
(use-package f
  :straight t)


;; Elisp Byte Compilation
(require 'comp)
(require 'compile)

(setq
 ;; if native-comp is having trouble, there's not very much I can do
 native-comp-async-report-warnings-errors 'silent
 ;; scroll to first error
 compilation-scroll-output 'first-error)



(provide 'environment-lisp)
;;; environment-lisp.el ends here.
