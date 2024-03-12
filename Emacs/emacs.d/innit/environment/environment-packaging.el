;;; environment-packaging.el --- Packages all the managers ;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   How Emacs packages are handled througout.
;;;   Rests upon `use-package' and `straight.el'.
;;;
;;; Code:

;; Package Management
;; The eponymous package manager is needed as a starting point
(require 'package)
;; Curb Emacs from eagerly loading packages immediately on startup.
(setq package-enable-at-startup nil)
;; Add other reknowned sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (setq package-archives
;;       (append package-archives
;; 	      '(("melpa" . "https://melpa.org/packages/")
;; 		("ublt"  . "https://elpa.ubolonton.org/packages/")
;;                 ("org"   . "https://orgmode.org/elpa/"))))

;; Enable byte-compilation of Elisp as standard.
(setq package-native-compile t)

;; Further, we rely on the wonderful ‘use-package’ system for most of this.
;; It’s part of Emacs core, as of version 29, so
(require 'use-package)
;; And enable the option to *ensure* packages are available, if not already
(require 'use-package-ensure)
;; Enable `use-package' to ensure other system items are ready installed.
;; <TODO: look into integration with nix & home-manager.
(require 'use-package-ensure-system-package)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;;   https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)
;; Always ensure that packages are downloaded and made available (if
;; not already installed).
(setq use-package-always-ensure t)


;; `straight.el' complements the other package management systems and assumes
;; Git repositories as the unit of a package.
;; (May consider Emacs 30's `package-vc-install' at a later date)
;; Bootstrapping straight.el
(setq straight-repository-branch "develop")
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





(provide 'environment-packaging)
;;; environment-packaging.el ends here.
