;;; workspace-completion.el --- Ways to get from start to finish  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:

;;; Orderless completion style (https://github.com/oantolin/orderless)
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;;; Consult:
;;; Mostly from: https://github.com/minad/consult#use-package-example
;;; TODO: consider auxilliary packages: https://github.com/minad/consult#auxiliary-packages
(use-package consult
  :straight t
  :bind ( ;; C-c bindings (mode-specific-map)
         ("C-c h" . #'consult-history)
         ("C-c m" . #'consult-mode-command)
         ("C-c k" . #'consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . #'consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . #'consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . #'consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . #'consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"   . #'consult-register-load)
         ("M-'"   . #'consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . #'consult-register)
         ;; Other custom bindings
         ("M-y" . #'consult-yank-pop)                ;; orig. yank-pop
         ("C-c C-h a" . #'consult-apropos)           ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e"   . #'consult-compile-error)
         ("M-g f"   . #'consult-flymake)             ;; Alternative: consult-flycheck
         ("M-g g"   . #'consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . #'consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . #'consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m"   . #'consult-mark)
         ("M-g k"   . #'consult-global-mark)
         ("M-g i"   . #'consult-imenu)
         ("M-g I"   . #'consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . #'consult-find)
         ("M-s D" . #'consult-locate)
         ("M-s g" . #'consult-grep)
         ("M-s G" . #'consult-git-grep)
         ("M-s r" . #'consult-ripgrep)
         ("M-s l" . #'consult-line)
         ("M-s L" . #'consult-line-multi)
         ("M-s m" . #'consult-multi-occur)
         ("M-s k" . #'consult-keep-lines)
         ("M-s u" . #'consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . #'consult-isearch-history)
         :map isearch-mode-map
         ("M-e"   . #'consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e" . #'consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . #'consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . #'consult-line-multi)            ;; needed by line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . #'consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . #'consult-history))                ;; orig. previous-matching-history-element
  :custom
  ;; sensible key for narrowing
  (consult-narrow-key (kbd ";"))
  ;; ensure ripgrep works
  (consult-project-root-function #'deadgrep--project-root)
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key '(:debounce 0.25 any))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.3
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Set preview key, first in general, then in context for specific commands
  (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))

;;; Vertico
;; “One final thing I have to do… and then I’ll be free of the past.”
(use-package vertico
  ;; note on how extensions are made known to `straight.el':
  ;;   https://kristofferbalintona.me/posts/202202211546/
  :straight (:files (:defaults "extensions/*")
                    :includes (vertico-indexed
                               vertico-flat
                               vertico-grid
                               vertico-mouse
                               vertico-quick
                               vertico-buffer
                               vertico-repeat
                               vertico-reverse
                               vertico-directory
                               vertico-multiform
                               vertico-unobtrusive))
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize t)
  :init
  (vertico-mode)
  ;; vertico completions occupy a separate, transient buffer,
  ;; rather than the minibuffer
  (vertico-buffer-mode)
  ;; vertico listens to a mouse (or trackpad 🐁)
  (vertico-mouse-mode)
  ;; use different layouts of vertico, as desired
  (vertico-multiform-mode)
  :bind (:map vertico-map
              ;; -directory keybindings
              ("?"     . #'minibuffer-completion-help)
              ("M-RET" . #'minibuffer-completion-help)
              ("TAB" . #'minibuffer-complete)
              ;; -multiform keybindings
              ("C-i" . #'vertico-quick-insert)
              ("C-o" . #'vertico-quick-exit)
              ("M-V" . #'vertico-multiform-vertical)
              ("M-G" . #'vertico-multiform-grid)
              ("M-F" . #'vertico-multiform-flat)
              ("M-R" . #'vertico-multiform-reverse)
              ("M-U" . #'vertico-multiform-unobtrusive)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Marginalia: the notes you wanted next to your text
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


(use-package company-auctex
  :after (company latex))


(use-package company-cabal
  :straight t
  :after (company haskell-cabal))

;; Corfu: a beautiful place, complete at point
;; see also: `emacs customisations in ui/ui-interaction.el
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (setq completion-category-overrides '((eglot (styles orderless))))
  (setq-default eglot-workspace-configuration
      '((haskell
         (maxCompletions . 200))))
  :init
  (global-corfu-mode))

;;;; And wear a Cape to Corfu
;; (Completion At PointE)
(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; Use Company backends as Capfs.
  (setq-local completion-at-point-functions
  (mapcar #'cape-company-to-capf
    (list #'company-files #'company-ispell #'company-dabbrev))))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'workspace-completion)
;;; workspace-completion.el ends here.
