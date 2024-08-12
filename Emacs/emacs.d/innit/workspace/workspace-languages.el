;;; workspace-languages.el --- Things That Add Fun to  Writing, Whatever the Language  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

;;; Tree Sitter makes working in many languages better.
;; This has recently become part of Emacs' core (as of 29):
;;   https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.
(use-package treesit
  :straight (:type built-in)
  :config
  ;; additional treesitter grammars added below
  (add-to-list 'treesit-language-source-alist '(nu "https://github.com/nushell/tree-sitter-nu"))
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst")))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; `treesit-auto' helps with migrating to the new {lang}-ts-mode instances:
;;   https://robbmann.io/posts/emacs-treesit-auto/
(use-package treesit-auto
  :straight (:type git :host github :repo "renzmann/treesit-auto")
  :demand t
  :defines treesit-auto--language-source-alist
  :custom (treesit-auto-install 'prompt)
  :config
  ;; NOTE: review this 'fallback-alist' from time to time
  (add-to-list 'treesit-auto-fallback-alist
               '(bash-ts-mode . sh-mode)
               '(c++-ts-mode . cc-mode))
  (global-treesit-auto-mode))


;;; x-refs as built in, with keybindings which mirror other common editors & IDE's
(use-package xref
  :bind (("C-<down-mouse-1>"   . #'xref-find-definitions)
         ("C-S-<down-mouse-1>" . #'xref-find-references)
         ("C-<down-mouse-2>"   . #'xref-go-back)
         ("s-r" . #'xref-find-references)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))


;;; Eglot. The tasteful language server. Now built-in to Emacs.
;;  with purported speedups from eglot-booster
;;  (which wraps https://github.com/blahgeek/emacs-lsp-booster)
(use-package eglot
  :after (yas-minor-mode ace-popup-menu)
  :commands (eglot eglot-code-actions eglot-rename)
  ;; these custom key bindings do not appear to work, still investigating
  ;; :bind (:map eglot-mode-map
  ;; 	      ("<mouse-3>" . eglot-code-actions)
  ;; 	      ("C-c a a"   . eglot-code-actions)
  ;; 	      ("C-c a r"   . eglot-rename))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-workspace-configuration
          '((haskell . ((formattingProvider . "fourmolu")))))
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  ;; eglot-send-changes-idle-time 0.1
  :hook ((c++-mode     . eglot-ensure)
         (c-mode       . eglot-ensure)
	 (elm-mode     . eglot-ensure)
         (go-mode      . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (python-mode  . eglot-ensure)
         (nix-mode     . eglot-ensure)
         (rust-mode    . eglot-ensure)))

(use-package eglot-booster
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))


;;; Flycheck: remove the lint from between the economy class seats
(use-package flycheck
  :straight t
;;  :blackout " 􀷰"
  :defines flycheck-define-checker
  :custom (flycheck-display-errors-delay 0.2)
  :init (global-flycheck-mode))

(use-package flymake-flycheck
  :straight t
  :hook
  (flymake-mode-hook . flymake-flycheck-auto))

;;; but ATempt To Repair At Point
;;
(use-package attrap
  :straight t
  :bind (("C-x /" . attrap-attrap)))

;;; ElDoc
(use-package eldoc
;;  :blackout " 􀈿"
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode))


;;; Spell-checking for the bountiful mess of human languages
;; use hunspell as the engine for Ispell
;; Note, by default, flyspell uses [[https://www.gnu.org/software/ispell/][Ispell]] by default (hence, needs installing separately, via ~brew~ or otherwise). On top of Ispell (or below it?) we use the highly popular [[http://hunspell.github.io][hunspell]].
(use-package ispell
  :init
  (setq-default ispell-program-name "hunspell")
  :custom (ispell-really-hunspell t))
;; Flyspell
(progn
  ;; automatically use Flyspell for text-like modes
  (mapc
   (lambda (mode) (add-hook mode #'flyspell-mode))
   '(org-mode-hook
     markdown-mode-hook
     text-mode-hook))
  ;; let it work with a Mac trackpad with a two-finger click
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))


;;; Snippets - just small fragments of language
(use-package yasnippet
  :straight t
;;  :blackout (yas-minor-mode . " 􁇔")
  :custom (yas-snippet-dirs '("~/DotFiles/Emacs/snippets/"))
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode)
         . yas-minor-mode-on)
  :config
  (yas-global-mode t))


;;; Agda
;; Using my own bastardised copy of Agda-mode, which allows for `agda2-mode'
;; to be imported in a consistent manner as other Emacs packages.
;; Importing `agda-mode' (well, `agda2-mode' at time of writing) is a little
;; atypical when using the standard method of installation (i.e. with Agda
;; itself). To circumvent this, and quell my addiction for `(use-package ...)'
;; expressions, we’re using a forked subtree of Agda’s ~emacs-mode~
;; subdirectory, with something like this:
;; `$ git subtree split -P src/data/emacs-mode -b emacs-mode-standalone'
;;   see https://stackoverflow.com/questions/359424/detach-move-subdirectory-into-separate-git-repository
;; But <TODO> next is to try is selfishly rearranging the Agda repo with [[https://github.com/josh-project/josh][Just One Single History]], /a.k.a/ [[https://josh-project.github.io/josh/intro.html][Josh]].
(use-package agda2-mode
  :load-path (lambda () (agda-mode-path))
  :init
  ;; almost always, we’ll want to use GHC
  (setq agda2-backend "GHC")
  ;; the info window for Agda seems a little small, this is to encourage it to be taller
  (setq agda2-information-window-max-height 0.5)
  ;; Activate agda-mode for regular and literate (markdown) Agda files
  :mode (("\\.agda\\'"     . agda2-mode)
         ("\\.lagda\\.\\(?:md\\|markdown\\|org\\|tex\\|latex\\|rst\\)\\'" . agda2-mode)))

;;; Haskell
;; with [[Dante][https://github.com/jyp/dante]]
;; (see also: http://h2.jaguarpaw.co.uk/posts/how-i-use-dante/)
(use-package haskell-mode
  :straight (haskell-mode :type git :host github :repo "haskell/haskell-mode")
  :bind (:map haskell-mode-map
              ("C-c a c" . haskell-cabal-visit-file)
              ("C-c a i" . haskell-navigate-imports)
              ("C-c m"   . haskell-compile)
              ("C-c a I" . haskell-navigate-imports-return)
              :map haskell-cabal-mode-map
              ("C-c m"   . haskell-compile))
  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-default)
  :custom
  (haskell-compile-cabal-build-command
   "cabal build all -ferror-spans -funclutter-valid-hole-fits")
  :hook
  (haskell-mode . eglot-ensure)
  (haskell-mode . (lambda () (setq eglot-confirm-server-initiated-edits nil)))
  (haskell-mode . (lambda ()
		    (flymake-mode)
		    (add-hook 'flymake-diagnostic-functions 'attrap-flymake-hlint nil t))))

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :config
  (require 'flymake-flycheck)
  (defalias 'flymake-hlint
    (flymake-flycheck-diagnostic-function-for 'haskell-hlint))
  (add-to-list 'flymake-diagnostic-functions 'flymake-hlint)
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  :hook
  (haskell-mode . flymake-mode)
  (haskell-mode . dante-mode))

(use-package haskell-snippets
  :straight t
;;  :blackout t
  :defer
  :after (haskell-mode yasnippet))

(use-package elm-mode
  :straight t)

;;; Lisp(s)
(use-package geiser-chez
  :straight t)

(use-package geiser-racket
  :straight t)

(use-package racket-mode
  :straight t)

(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t)


;;; Rust
;; …a wonderful imperative language with the most ergonomic memory abstraction
;; imaginable, which is understood by having crabs claw at your brain.
;; The inbuilt (tree-sitter enabled) Rust mode suffices well.
(use-package rust-ts-mode
  :hook (rust-ts-mode . eglot-ensure))
;; also cargo-mode offers a more ergonomic Emacs interface to cargo
(use-package cargo-mode
  :straight t
  :hook
  (rust-mode . cargo-minor-mode))


;; Python
(use-package python
  ;; encouraging use of the tree-sitter-powered 'ts' modes
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :init
  ;; Ruff is a welcome addition to the Python ecosystem.
  ;; Let's add it as a linter via Flycheck (from: https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202)
  (flycheck-define-checker python-ruff
    "Python syntax and style checker using Ruff."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes (python-mode python-ts-mode))
  (add-to-list 'flycheck-checkers 'python-ruff)
  ;; A language server needs to be available for eglot and there are many
  ;; to choose from. The current preference is `jedi-language-server',
  ;; available via `pip'. This then seems to 'just work' out of the box.
  :hook  ((python-mode . eglot-ensure)
          (python-ts-mode . eglot-ensure))
  :custom (python-indent-offset 4))

;; Poetry for Python
(use-package poetry
 :straight t)


;;; C & C++
;; TODO: c++-ts-mode currently causes some confusion with tree sitter ('c++' or 'cpp'?)
;;       come back to this at a later date... (2023-02-03)
(use-package c-ts-mode
  ;; :init
  ;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure))


;; Golang
(use-package go-mode
  :straight t)


;;; European Computer Manufacturers Association Scripts
;; Really, that’s what ECMAScript stands for. JavaScript, TypeScript and all that
;; Note for clarity below:
;;  'js' stands for JavaScript,
;;  'ts' stands for TypeScript,
;;  'ts' stands for Tree Sitter,
;;  'sic' stants for "sic erat scriptum"
(use-package typescript-ts-mode
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-ts-mode
    "TypeScript TSX")
  ;; use our derived mode for tsx files
  :mode ("\\.tsx?\\'" . typescriptreact-mode)
  :interpreter "deno"
  :custom (typescript-indent-level 2)
  :hook ((typescript-ts-mode
          typescriptreact-mode)
         . eglot-ensure))


;;; Nix
(use-package nix-mode
  :straight t
  :after eglot
  :mode ("\\.nix\\'" . nix-mode)
  :config
  ;; use nil as the language server for Nix (https://github.com/oxalica/nil) <TODO: ensure nil is installed>
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook (nix-mode . eglot-ensure))


;;; Dhall
(use-package dhall-mode
  :straight t)


;; ... and the Argonauts
(use-package json-ts-mode
  :mode ("\\.\\(?:json\\|jsonc\\|jsonp\\|geojson\\)" . json-ts-mode))

;;; Shells 🐚
;; Fish
(use-package fish-mode
  :straight t)
;;  :blackout "􁖐︎")
;; the other ones (Bourne-compatible)
(use-package sh-script
  :mode
  (("bashrc$" . sh-mode)
   ("zshrc$"  . sh-mode))
  :hook (sh-mode . eglot-ensure))

;; Nu 🐚
(use-package nushell-ts-mode
  :straight (nushell-ts-mode :type git :host github :repo "herbertjones/nushell-ts-mode")
  :mode ("\\.nu\\'" . nushell-ts-mode)
  ;; :config
  ;; (require 'nushell-ts-babel)
  ;; (defun hfj/nushell/mode-hook ()
  ;;   (corfu-mode 1)
  ;;   (highlight-parentheses-mode 1)
  ;;   (electric-pair-local-mode 1)
  ;;   (electric-indent-local-mode 1))
  ;; (add-hook 'nushell-ts-mode-hook 'hfj/nushell/mode-hook))
  )

;;; GraphViz
(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 4))

;;; Τεχ
(use-package auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  ;; language server provided by `digestif'   (via nix from lua53Packages.digestif)
  :hook (tex-mode . eglot-ensure))

;;; Typst
(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))


;;; Markdown
(use-package markdown-mode
  :straight t
  :mode
  (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :hook
  ((markdown-mode . variable-pitch-mode)
   (markdown-mode . rainbow-mode))
  :custom
  (markdown-command "pandoc -t html5"))

(use-package markdown-preview-mode
  :straight t
  :config
  (setq markdown-preview-stylesheets
	'("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"))
  (add-to-list 'markdown-preview-javascript
	       "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))


;; Org Mode
(use-package org
  :custom
  (org-image-actual-width nil))


;;; Text. Plain or otherwise.
(use-package titlecase
  :straight (:type git :host codeberg :repo "acdw/titlecase.el")
  :custom (titlecase-style 'nyt))


;; If rainbows are the language of colours, this can go here
(use-package rainbow-mode
  :straight t
;;  :blackout " 􀇗"
  :hook (conf-mode
         prog-mode
         text-mode))


(provide 'workspace-languages)
;;; workspace-languages.el ends here.
