;; init.el --- Emacs Setup -----------------------
;;; Commentary:
;;; This file does initial setup, prior to loading `readme.org` via org-babel
;;; credit due to: <https://github.com/patrickt/emacs/blob/master/init.el>
;; -*- coding: utf-8; lexical-binding: t -*-

;; Clean up chrome
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


;;; Basic package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


;;;; JIT Thyself
;; JIT packages
(setq package-native-compile t)
;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

(setq max-lisp-eval-depth 2000)


;;;; Package Management with `straight.el'
;;
;; Bootstrap the package manager, `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; use SSH for fetching remote Git repos; allows access to private ones  
(setq straight-vc-git-default-protocol 'ssh)


;;;; use-package with straight.el
;;
;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(eval-when-compile
  (require 'straight)
  (require 'use-package)
  (require 'ob-tangle))

(defun reload-init ()
  "Reload the literate config via org-babel"
  (interactive)
  (org-babel-load-file "~/DotFiles/Emacs/emacs.org-tangle.d/readme.org"))

(reload-init)
(provide 'init)
;; init.el ends here  -- and if anything appends below the line, remove it! (and politely place it in readme.org)
