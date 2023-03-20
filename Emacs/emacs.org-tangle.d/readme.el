;; -*- coding: utf-8; lexical-binding: t -*-

(defun open-init-file ()
  "Open the great org config file for your Emacs."
  (interactive)
  (find-file "~/DotFiles/Emacs/emacs.d/readme.org")
  (auto-revert-mode))

(defun reload-init-file ()
  "Reload the great org config file and reflect changes in current buffer
note that you're probably in a buffer editing this file when you do it"
  (interactive)
  ;; close and reopen this file if we're visiting it (in the current buffer <TODO> change to if visting in this session)
  (if (string-match-p
       (regexp-quote "DotFiles/Emacs/emacs.d/readme.org") (buffer-file-name))
      (progn
        (save-buffer)
        (kill-this-buffer)
        (reload-init)
        (open-init-file))
    ;; otherwise just reload Emacs init 'in the background'
    (reload-init)))

(bind-key "C-c i o" #'open-init-file)
(bind-key "C-c i r" #'reload-init-file)

(setq gc-cons-threshold (* 100 1000 1000))
(setq large-file-warning-threshold (* 100 1000 1000))
(setq max-specpdl-size 5000)

(require 'cl-lib)

(setq use-package-always-ensure t)

;; Check for fish and provide it to anyone with a taste for it
(defvar local/fish-location
  (string-trim-right (shell-command-to-string "which fish")))

(use-package exec-path-from-shell
  :straight t
  :init
  ;; Use fish if we have found one at the market (or on the PATH)
  (when (file-exists-p local/fish-location)
    (setq exec-path-from-shell-shell-name local/fish-location))
  ;; list all the environment variables to carry over to here
  (setq exec-path-from-shell-variables
        '("PATH"
          "MANPATH"
          "INFOPATH"
          "LANG"
          "LSCOLORS"
          "HOMEBREW_PREFIX"
          "PYENV_ROOT"
          "SSH_AUTH_SOCK"
          "SSH_AGENT_PID"))
  :config
  ;; initialise if were in the daemon process or a sole MacOS process
  (when (or
         (daemonp)
         (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; A double-space after a full-stop  is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; scroll to first error
 compilation-scroll-output 'first-error
 ;; accept 'y' or 'n' instead of yes/no because too much typing
 use-short-answers t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; if native-comp is having trouble, there's not very much I can do
 native-comp-async-report-warnings-errors 'silent
 ;; unicode ellipses are better
 truncate-string-ellipsis "…")

(setq-default indent-tabs-mode nil)

(setq default-directory "~/Projects/")

(setq custom-file (make-temp-name "/tmp/"))

(setq
  make-backup-files nil
  auto-save-default nil
  create-lockfiles nil)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key [remap kill-whole-line] #'smart-kill-whole-line)

(setq ns-use-proxy-icon +1)

(straight-use-package 'use-package)
(use-package el-patch
  :straight t)

(use-package tao-theme
  :straight t
  :custom
  (tao-theme-use-sepia t)
  (tao-theme-sepia-saturation 1.03)
  (tao-theme-sepia-depth 7)
  (tao-theme-use-boxes nil)
  :init
  (load-theme 'tao-yang t))

(setq local/theme/palette (tao-theme-yang-palette))

(setq local/theme/colours
      '((background    . (cdr (assoc "color-4"  local/theme/palette)))
        (fg-extra-mild . (cdr (assoc "color-6"  local/theme/palette)))
        (fg-more-mild  . (cdr (assoc "color-7"  local/theme/palette)))
        (fg-midi-mild  . (cdr (assoc "color-8"  local/theme/palette)))
        (fg-mild       . (cdr (assoc "color-9"  local/theme/palette)))          
        (fg-deep       . (cdr (assoc "color-15" local/theme/palette)))
        (mode-line-bg  . (cdr (assoc "color-6"  local/theme/palette)))
        (mode-line-fg  . (cdr (assoc "color-12" local/theme/palette)))
        (mode-line-ibg . (cdr (assoc "color-4"  local/theme/palette)))
        (mode-line-ifg . (cdr (assoc "color-8"  local/theme/palette)))
        (highlight     . "#fefee6")))

(defun local/theme/colour (name)
  (eval (cdr (assoc name local/theme/colours))))

(use-package taoline
  :straight (:type git :host github :repo "11111000000/taoline")
  :bind* ("C-x t" . #'taoline-mode)
  :custom
  (setq taoline-show-buffer-name     t
        taoline-show-git-branch      t
        taoline-show-dir             t
        taoline-show-previous-buffer t
        taoline-show-time            nil)
  :config
  (taoline-mode t))

(use-package blackout
  :straight t)

(custom-set-faces
 `(mode-line
   ((t (:background ,(local/theme/colour 'mode-line-bg)
                    :family "SF Mono"
                    :size 120
                    :foreground ,(local/theme/colour 'mode-line-fg)
                    :box (:line-width 3 :color ,(local/theme/colour 'mode-line-bg))))))
 `(mode-line-inactive
   ((t (:background ,(local/theme/colour 'mode-line-ibg)
                    :family "SF Mono"
                    :size 120
                    :foreground ,(local/theme/colour 'mode-line-ifg)
                    :box (:line-width 3 :color ,(local/theme/colour 'mode-line-ibg))))))
 `(mode-line-highlight
   ((t (:background ,(local/theme/colour 'highlight)
                    :family "SF Mono"
                    :size 120
                    :foreground ,(local/theme/colour 'fg-deep)
                    :box (:line-width 3 :color ,(local/theme/colour 'highlight)))))))

(set-face-foreground 'vertical-border (local/theme/colour 'mode-line-bg))

(fringe-mode 12)
(global-visual-line-mode +1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(blackout 'visual-line-mode " 􀑉")

;; Use our favourit fixed-width font (a function, which will allow this to be defined conditionally
(defun local/fixed-width-font-family ()
    "MonoLisa")
;; Symbols is such a wide-ranging set
(set-fontset-font t 'symbol (local/fixed-width-font-family))
(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
(set-fontset-font t 'symbol "Menlo" nil 'append)
(set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'append)
;; More pretty items via SF Symbols 􀪱, believed to be in Unicode range [u00100000,u00101741]
(set-fontset-font t '(#x100000 . #x101800) "SF Compact")

;; かな
(set-fontset-font t 'kana "Hiragino Sans")
;; 漢字
(set-fontset-font t 'han "Hiragino Sans")
;; ᨆᨀᨔᨑ
(set-fontset-font t 'makasar "Noto Serif Makasar")

;; There are a few symbols missing from the MonoLisa repertoire
;; – all in the Latin Extended Additional block – so are patched here in Menlo
(set-fontset-font t '(#x001e00 . #x001e9f) "Menlo")

;; International rescue font
(set-fontset-font t nil "Noto Sans")

(set-face-attribute 'default nil
                    :family  (local/fixed-width-font-family)
                    :font    (local/fixed-width-font-family)
                    :height   140
                    :inherit 'default)

(set-face-attribute 'fixed-pitch nil
                    :inherit 'default)

(set-face-attribute 'variable-pitch nil
                    :font "Cormorant Infant"
                    :height 180
                    :inherit 'default)

(use-package ligature
  :straight t
  :config
  (let (
        ;; Ligatures to use in /any/ mode
        (ligatures-for--all '("www"))
        ;; Ligatures to use in writing and more prose-based modes,
        ;; if the `variable-pitch' face supports them
        (ligatures-for--writing '("ff" "fi" "ffi"))
        ;; Ligatures to use in programming modes
        (ligatures-for--programming
         '("www"   "**"    "***"   "**/"   "*>"    "*/"    "\\\\"  "---"
           "{-"    "-}"    "{|"    "|}"    "[|"    "|]"    "'''"   "\"\"\""
           "::"    ":::"   ":="    "::="   "!!"    "=="    "==="   "!="    
           "!=="   "=!="   "~="    "..="   "=:="   "#="    "----"  "-->"   
           "->"    "->>"   "-<"    "--<"   "-<<"   "<-<"   ">->"   "-~"    
           "<~>"   "#{"    "#["    "]#"    "##"    "###"   "####"  "#("    
           "#?"    "#_"    "#_("   ".-"    ".="    ".."    "..<"   "..."   
           ".?"    "?="    "??"    ";;"    "/*"    "/**"   "/="    "/=="   
           "/>"    "//"    "///"   "||"    "||="   "|="    "=/"    "|>"    
           "^="    "$>"    "++"    "+++"   "+>"    "==>"   "=>"    "=>>"   
           "<==>"  "<="    "=<<"   "=/="   ">-"    ">="    ">=>"   ">>"    
           ">>-"   ">>="   ">>>"   "<*"    "<*>"   "|||"   "<|"    "<||"   
           "<|||"  "<|>"   "<$"    "<$>"   "<!--"  "<-"    "<--"   "<->"   
           "<+"    "<+>"   "<="    "<=="   "<=>"   "<=<"   "<>"    "<<"    
           "<<-"   "<<="   "<<<"   "<~"    "<~~"   "</"    "</>"   "<:"    
           ":<"    ":>"    "~@"    "~-"    "~>"    "~~"    "~~>"   "%%"    
           "&&"    "!!."   "!!!"
           )))

    (ligature-set-ligatures 't ligatures-for--all)
    (ligature-set-ligatures 'org-mode  ligatures-for--writing)
    (ligature-set-ligatures 'text-mode ligatures-for--writing)
    (ligature-set-ligatures 'prog-mode ligatures-for--programming))

  :init
  (global-ligature-mode t))

(use-package face-remap
  :straight (:type built-in)
  :blackout (buffer-face-mode . " 􀥧"))

(use-package olivetti
  :straight t
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 120
        olivetti-minimum-body-width 80))

(straight-use-package 'dimmer)
;; attempt to prevent _weird_ interaction with ~tao-theme~ (when using sepia):
(setq dimmer-use-colorspace :cielab)
(setq dimmer-fraction 0.76)
;; (setq dimmer-adjustment-mode :foreground)
(dimmer-configure-magit)
(dimmer-configure-org)
(dimmer-configure-which-key)

(setq dimmer-watch-frame-focus-events nil)
;; Leave the lights on in Neotree
(add-to-list
 'dimmer-exclusion-regexp-list "\\*NeoTree**\\*")
;; And in ElDocs
 (add-to-list
  'dimmer-exclusion-regexp-list "\\*eldoc**\\*")
 (add-to-list
  'dimmer-exclusion-regexp-list "\\*EGLOT**\\*")

(defun advices/dimmer-config-change-handler ()
  (dimmer--dbg-buffers 1 "dimmer-config-change-handler")
  (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                         dimmer-prevent-dimming-predicates)))
    (dimmer-process-all (not ignore))))

(advice-add 'dimmer-config-change-handler
            :override #'advices/dimmer-config-change-handler)
;; dim!
(dimmer-mode)

(use-package rainbow-mode
  :straight t
  :blackout " 􀇗"
  :init
  (rainbow-mode))

(use-package ns-auto-titlebar
  :straight t)
(when (eq system-type 'darwin)
  (ns-auto-titlebar-mode))

(setq frame-title-format
      '(multiple-frames "%b"
                        ("" invocation-name "@" system-name)))

(when (equal system-type 'darwin)
 ;; soon we'll need to bind the Apple Ultra key, no doubt...
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier  'meta)
  (setq ns-function-modifier 'hyper)
  (straight-use-package 'redo)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
(define-key key-translation-map (kbd "<s-down-mouse-1>") (kbd "<down-mouse-2>"))

;; ;; Move focus to new Emacs frame on creation (for MacOS)
;; (when (featurep 'ns)
;;   (defun ns-raise-emacs ()
;;     "Raise Emacs."
;;     (ns-do-applescript "tell application \"Emacs\" to activate"))
;;   (defun ns-raise-emacs-with-frame (frame)
;;     "Raise Emacs and select the provided frame."
;;     (with-selected-frame frame
;;       (when (display-graphic-p)
;;         (ns-raise-emacs))))
;;   (add-hook #'after-make-frame-functions #'ns-raise-emacs-with-frame)
;;   (when (display-graphic-p)
;;     (ns-raise-emacs)))

(when (equal system-type 'darwin)
  (setq dired-use-ls-dired nil))

;; ⌘ W (s-w) deletes a window
(global-set-key (kbd "s-w") #'delete-window)
;; ⇧⌘ W (s-W) deletes a frame
(global-set-key (kbd "s-W") #'delete-frame)
;; ⌥⌘ W (M-s-W) deletes a window **and** the buffer therein (see below)
(global-set-key (kbd "M-s-w")
                #'(lambda () (maybe-delete-frame-buffer (selected-frame))))
;; ⌥⇧⌘ W (M-s-W) deletes a frame, thereby all frames within **and** all buffers in said frames

;; ⌘ N (s-n) creates a new frame visiting the scratch buffer (default is to create a new frame visiting wherever you just were, which is rarely what you want and slow for more ‘involved’ buffers.is already bound to '(make-frame)
(global-set-key (kbd "s-n")
                #'(lambda ()
                    (interactive)
                    (make-frame)
                    (switch-to-buffer "*scratch*")))

;; fn-f is usually full screen mode on MacOS, hyper == fn, so:
(global-set-key (kbd "H-f") #'toggle-frame-fullscreen)

;; Emacs has a nice interface for inserting Unicode chars by description; MacOS muscle memory links this to fn-E
(global-set-key (kbd "H-e") #'insert-char)

(defun maybe-delete-frame-buffer (frame)
    "When a dedicated FRAME is deleted, also kill its buffer.
  A dedicated frame contains a single window whose buffer is not
  displayed anywhere else."
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (eq 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))

(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
    (server-start))

(use-package tramp
  :straight (:type built-in)
  :config
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        tramp-default-remote-shell "/bin/bash"
        tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/bash")
          (shell-command-switch "-c")))

  (connection-local-set-profile-variables 'tramp-connection-local-default-shell-profile
                                        '((shell-file-name . "/bin/bash")
                                          (shell-command-switch . "-c"))))

(use-package savehist
  :init
  (savehist-mode))

(save-place-mode 1)

(delete-selection-mode t)
;; (bind-key* "<backspace>" #'delete-region)
;; (bind-key* "S-<backspace>" #'backward-delete-char-untabify)

(use-package command-log-mode
  :straight t)

(use-package which-key
  :straight t)
(with-eval-after-load 'which-key
  (blackout 'which-key-mode " 􀓖"))
(which-key-mode)

;; bound to (C-x o) by default, which is fine, but something quicker and more Mac-like is pleasant
(bind-key "C-<tab>" #'other-window)

(global-set-key (kbd "C-s-w") #'window-toggle-side-windows)

(global-set-key (kbd "C-s-_" ) #'shrink-window)
(global-set-key (kbd "C-s-+" ) #'enlarge-window)
(global-set-key (kbd "C-s--" ) #'shrink-window-horizontally)
(global-set-key (kbd "C-s-=" ) #'enlarge-window-horizontally)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(bind-key "C-c b s" #'switch-to-scratch-buffer)

(use-package recentf
  :straight t
  :config
  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items 50)
  ;; exclude directories of files which are frequently visited in the background
  (add-to-list 'recentf-exclude
               '("\\elpa"
                 "\\straight"
                 "private/tmp"
                 ))
  ;; Periodically save recent files, every 5 minutes
  (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; …and that means once every 5 minutes for each file that is being visited
  ;; and is saveable, so quiescing "wrote ... recentf" messages is sane.
  ;; this SO post has a solution [https://emacs.stackexchange.com/a/68323]
  (defun suppress-msg (function)
    "Prevent FUNCTION from showing `Wrote <FILE>' messages.
    (The messages are still logged to `*Messages*'.)"
    (let ((inhibit-message  t))
      (funcall function)))
  (advice-add 'recentf-save-list :around 'suppress-msg)

  (recentf-mode +1))

;; Some magic with the compile package - presumably convenient when 'compiling' projects, or just reducing the noise thereof...
(use-package compile
  :straight t
  :custom
  (compilation-read-command nil "Don't prompt every time.")
  (compilation-scroll-output 'first-error))

;; Helper function for those hard-to-find project roots
(defun project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file.

 If this file exists, it marks the project root. For convenient compatibility
 with Projectile, '.projectile' is also considered a project root marker.

 https://blog.jmthornton.net/p/emacs-project-override"
  (let ((root (or (locate-dominating-file dir ".project.el")
                  (locate-dominating-file dir ".projectile")))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                   (cons 'vc root)
                 (list 'vc backend root)))))
;; Configure the built in `project.el' package
;; It's built-in, but is desirable to keep up-to-date with the latest GNU
;; release. Also, other packages 'expect' project.el to be loaded,
;; so load eagerly with `:demand'
(use-package project
  :straight t
  :pin gnu
  :demand t
  :bind (("C-c k" . #'project-kill-buffers)
         ("C-c m" . #'project-compile)
         ("C-x f" . #'find-file)
         ("C-c f" . #'project-find-file)
         ("C-c F" . #'project-switch-project)
         ("C-c R" . #'local/recentf-in-project))
  :custom
  (project-switch-commands
   '((project-find-file "Find File")
     (magit-project-status "Magit" ?g)
     (deadgrep "Grep" ?h)
     (local/vterm-for-project "vterm" ?t)
     (project-dired "Dired" ?d)
     (local/recentf-in-project "Recently Opened" ?r)))
  (compilation-always-kill t)
  (project-vc-merge-submodules nil)
  ;; And include the helper function above as a hook.
  ;; Note that we cannot use :hook here because `project-find-functions' doesn't
  ;; end in "-hook", and we can't use this in :init because it won't be defined
  ;; yet.
  :config
  (add-hook 'project-find-functions #'project-root-override))

(defun local/recentf-in-project ()
  "As `recentf', but filtered based on the current project root [credit: patrickt]"
  (interactive)
  (let* ((proj (project-current))
         (root (if proj (project-root proj) (user-error "Not currently in a project."))))
    (cl-flet ((ok (fpath) (string-prefix-p root fpath)))
      (find-file (completing-read "Find recent file:" recentf-list #'ok)))))

(setq enable-local-variables :all)

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
:init
;; set buffers to be handled by Popper
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*deadgrep.\*"
        "\\*eldoc.*\\*"
        "\\*direnv\\*"
        "\\*xref\\*"
        "\\*Process List\\*"
        help-mode
        occur-mode
        ;; compliation-related
        compilation-mode
        haskell-compilation-mode
        "\\*haskell\\*"
        "\\*cargo-run\\*"
        "\\*rustic-compilation\\*"
        "\\*rustfmt\\*"
        "\\*Go Test\\*"
        "\\*interpretation\\*"
        "\\*Quail Completions\\*"
        ;; shell modes
        "^\\*eshell.*\\*$" eshell-mode
        "^\\*shell.*\\*$"  shell-mode 
        "^\\*term.*\\*$"   term-mode  
        "^\\*vterm.*\\*$"  vterm-mode))
;; Other variables for Popper
(setq popper-group-function #'popper-group-by-project
      popper-mode-line '(:eval (propertize " 🫧" 'face 'mode-line-emphasis)) ; cute ;)
      popper-window-height 22)
(popper-mode +1)
(popper-echo-mode +1))

(use-package try
  :straight t)

(use-package neotree
  :straight t
  :init
  ;; Emacs project.el (built-in) integration with Neotree
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (nth 2 (project-current)))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  :custom
  ;; Use arrow, but not icons
  (neo-theme 'nerd)
  ;; since we're working on MacOS only (for now)
  (neo-default-system-application "open")
  ;; Open Neotree tree at current file
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-window-width 30)
  ;; leaving the neotree window as a fixed size tends to work better with resizing and line-wrapping
  (neo-window-fixed-size t)  ;; default
  ;; Reflect VC state in the tree
  (neo-vc-integration '(face char))
  :bind*
  ;; something that reminds me of Safari's sidebar
  (("s-L" . #'neotree-project-dir)))

(add-hook
 'neotree-mode-hook
 (lambda ()
   ;; use buffer-face-mode for main colours
   (setq buffer-face-mode-face
         `(:background ,(local/theme/colour 'background)
           :foreground ,(local/theme/colour 'fg-mild)
           :family "SF Mono"
           :weight light))
   ;; use custom faces for item-specific, specific faces
   (custom-set-faces
    `(neo-root-dir-face
      ((t (:background ,(local/theme/colour 'background)
           :foreground ,(local/theme/colour 'fg-mild)
           :weight bold))))
    ;; files marked as ‘edited’ by version control
    '(neo-vc-edited-face
      ((t (:foreground "#b58900"))))
    ;; files not yet tracked by version contol
    '(neo-vc-unregistered-face
      ((t (:foreground "#22aa22")))))
   (buffer-face-mode 1)))

(add-hook 'neo-after-create-hook
          #'(lambda (_)
              (with-current-buffer (get-buffer neo-buffer-name)
                (setq truncate-lines t)
                (setq word-wrap nil)
                (make-local-variable 'auto-hscroll-mode)
                (setq auto-hscroll-mode nil))))

(use-package autorevert
  :straight (:type built-in)
  :blackout (auto-revert-mode . " 􀉖")
  :init
  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  ;; Revert Dired and other buffers
  (setq global-auto-revert-non-file-buffers t))

(use-package vundo
  :straight t
  :bind*
  (("C-c _" . vundo)
   ("C-s-z" . vundo))
  :custom vundo-glyph-alist vundo-unicode-symbols
  :custom-face (vundo-highlight ((t (:inherit :foreground "#414141")))))

(global-set-key (kbd "s-Z") #'undo-redo)

;; Line Numbers
;;; Make easy on the eyes
(set-face-attribute 'line-number nil
                    :font "SF Mono"
                    :weight 'light
                    :foreground (local/theme/colour 'fg-more-mild))
(set-face-attribute 'line-number-current-line nil
                    :font "SF Mono"
                    :weight 'medium
                    :foreground (local/theme/colour 'fg-mild))

;;; and sensibly fancy
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 5
              display-line-numbers-widen t)

;;; Disable in default modes
(line-number-mode -1)
(global-display-line-numbers-mode -1)

;;; but enable for programming, text editing, and Org modes
(mapc
 (lambda (mode) (add-hook mode #'display-line-numbers-mode))
 '(agda2-mode
   conf-mode-hook
   haskell-cabal-mode-hook
   makefile-hook
   nix-mode
   org-mode-hook
   prog-mode-hook
   python-mode-hook
   sh-mode-hook
   text-mode-hook))

;; Let's also highlight the current line in similar situations
;; disabling for now to see how things fare without this contrivance
(use-package hl-line
  :straight (:type built-in)
  :disabled t
  :config
  (custom-set-faces
   `(hl-line ((t (:background ,(local/theme/colour 'highlight))))))
  :hook ((text-mode
          prog-mode
          org-mode)
         . hl-line-mode))

(column-number-mode +1)

(use-package vertico
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
  ;; vertico completions occupy a separate, transient buffer (rather than the minibuffer)
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

;; Configure directory extension.
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

(use-package emacs
  :init
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
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28 and thereafter: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(setq completion-styles '(basic substring partial-completion flex))

(use-package consult
  :straight t
  :bind (
         ;; C-c bindings (mode-specific-map)
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

  ;; The :init configuration is always executed (Not lazy)
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
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

(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; and some useful config for Emacs for Corfu, per Minad's recommendations
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  ;; compatibility with Emacs 28..
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  ;; Enable completion at point with <TAB>
  ;; first <TAB> will indent, subsequent will attempt to autocomplete
  (setq tab-always-indent #'complete))

(use-package embark
  :bind ("C-c e" . #'embark-act)
  :bind ("C-<escape>" . #'embark-act))
(use-package embark-consult :after (embark consult))

(use-package paredit
  :straight t
  :config
  :hook
  ((emacs-lisp-mode-hook . paredit-mode)
   (racket-mode-hook . paredit-mode)
   (racket-repl-mode-hook . paredit-mode))
  :bind (:map paredit-mode-map
              ("{" . #'paredit-open-curly)
              ("}" . #'paredit-close-curly)
              ("M-[" . #'paredit-wrap-square)
              ("M-{" . #'paredit-wrap-curly)))

(use-package multiple-cursors
  :straight t
  ;; `C-s-c' is keystroke of entry for Multiple Cursor actions
  :bind (("C-s-c C-s-c" . #'mc/edit-lines)
         ("C-s-c C-n"   . #'mc/mark-next-like-this)
         ("C-s-c C-p"   . #'mc/mark-previous-like-this)
         ("C-s-c C-f"   . #'mc/mark-next-like-this-word)
         ("C-s-c C-p"   . #'mc/mark-previous-like-this-word)
         ("C-s-c s-a"   . #'mc/mark-all-like-this)
         ;; I'll never remember that the control sequence for a
         ;; carriage return is `C-j` so must allow use of the
         ;; Return key in all contexts, including here
         ("<return>" . nil))
  :config
  ;; integrate with MacOS clipboard when copying at multiple cursors
  (setq mc/cmds-to-run-for-all '(ns-copy-including-secondary)))

(use-package yasnippet
  :straight t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode)
         . yas-minor-mode-on)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(with-eval-after-load 'yasnippet
  (blackout 'yas-minor-mode " 􀉈"))

(add-hook 'before-save-hook 'time-stamp)

(use-package anzu
  :straight t
  :blackout " 杏")
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(use-package deadgrep
  :straight t
  :bind* (("<f5>" .  #'deadgrep)
          ("H-s-f" . #'deadgrep)))

(global-set-key (kbd "C-s-f") #'list-matching-lines)

(use-package magit
  :straight t
  :config (setq auto-revert-check-vc-info t))

(use-package libgit
  :straight t
  :after magit)

(use-package magit-libgit
  :disabled
  :after (magit libgit))

;; Integration with Github and other so-called "forges"
(use-package forge
  :after magit)

;; hack to eliminate weirdness
(unless (boundp 'bug-reference-auto-setup-functions)
  (defvar bug-reference-auto-setup-functions '()))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode t)
  (diff-hl-dired-mode t)

  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package magit-delta
  :straight t
  :hook (magit-mode . magit-delta-mode))

(use-package vterm
  :straight t
  :custom
  ;; don't ask to compile the vterm C module if it's not already compiled
  (vterm-always-compile-module t)
  ;; update state more frequenlty than the default
  (vterm-timer-delay 0.05)
  ;; swim with the fish
  (vterm-shell local/fish-location)
  :config
  (defun local/vterm-for-project ()
    "Invoke `vterm' in the current project root.
Switch to the project-specific term buffer if it already exists."
    (interactive)
    (let* ((project (project-current))
           (buffer  (format "*vterm %s*" (consult--project-name (project-root project)))))
      (unless (buffer-live-p (get-buffer buffer))
        (unless (require 'vterm nil 'noerror)
          (error "Package `vterm' is not available"))
        (vterm buffer)
        (vterm-send-string (concat "cd " (project-root project)))
        (vterm-send-return))
      (switch-to-buffer buffer))))

  (use-package vterm-toggle
    :straight t
    :custom
    (vterm-toggle-fullscreen-p nil "Open a vterm in a another window.")
    (vterm-toggle-scope 'project)
    ;; allow ⇧⌘T to be a global shortcut for showing/hiding vterm
    :bind* ("C-s-t" . #'vterm-toggle)
    :bind (:map vterm-mode-map
                ("s-T" . #'vterm) ;; new tab
                ("s-v" . #'vterm-yank)
                ;; why choose.
                ("C-y" . #'vterm-yank)))

(use-package xref
  :pin gnu
  :bind (("C-<down-mouse-1>"   . #'xref-find-definitions)
         ("C-S-<down-mouse-1>" . #'xref-find-references)
         ("C-<down-mouse-2>"   . #'xref-go-back)
         ("s-r" . #'xref-find-references)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

(use-package eldoc
  :pin gnu
  :blackout " 􀩴…"
  :bind ("s-d" . #'eldoc)
  :custom
  (eldoc--echo-area-prefer-doc-buffer-p t)
  (eldoc-echo-area-use-multiline-p t))

(use-package dash-at-point
  :bind ("C-c D" . dash-at-point))

(use-package eglot
  :hook ((c++-mode     . eglot-ensure)
         (c-mode       . eglot-ensure)
         (go-mode      . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (python-mode  . eglot-ensure)
         (rust-mode    . eglot-ensure)
         (nix-mode     . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-<down-mouse-1>" . #'xref-find-definitions)
              ("C-s-<down-mouse-1>" . #'xref-find-references)
              ("C-c a r" . #'eglot-rename)
              ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  :config
  ;; Use Nil (https://github.com/oxalica/nil) for Nix
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  ;; Eglot doesn't correctly unescape markdown: https://github.com/joaotavora/eglot/issues/333
  (defun mpolden/gfm-unescape-string (string)
    "Remove backslash-escape of punctuation characters in STRING."
    ;; https://github.github.com/gfm/#backslash-escapes
    (replace-regexp-in-string "[\\\\]\\([][!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-]\\)" "\\1" string))

  (advice-add 'eglot--format-markup :filter-return 'mpolden/gfm-unescape-string)

  (defun pt/add-eglot-to-prog-menu (old startmenu click)
    "Add useful Eglot functions to the prog-mode context menu."
    (let ((menu (funcall old startmenu click))
          (identifier (save-excursion
                        (mouse-set-point click)
                        (xref-backend-identifier-at-point
                         (xref-find-backend)))))
      (when identifier
        (define-key-after menu [eglot-find-impl]
          `(menu-item "Find Implementations" eglot-find-implementation
                      :help ,(format "Find implementations of `%s'" identifier))
          'xref-find-ref))
      menu))

  (advice-add 'prog-context-menu :around #'pt/add-eglot-to-prog-menu))

(use-package consult-eglot
  :bind (:map eglot-mode-map ("s-t" . #'consult-eglot-symbols)))

(use-package flymake
  :straight t
  :blackout " 􀑓"
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)))

(use-package flycheck
  :straight t
  :blackout " 􀷰"
  :init (global-flycheck-mode))

(use-package tree-sitter
  :straight t
  :blackout " 🎄"
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t)

;; use hunspell as the engine for Ispell
(setq-default ispell-program-name "hunspell")
(setq ispell-really-hunspell t)

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

;; Using my own bastardised copy of Agda-mode from github
(use-package agda2-mode
  :straight (:type git :host github :repo "paulroden/agda-mode")
  :hook ((text-mode prog-mode) . (lambda () (set-input-method "Agda")))
  :init
  ;; almost always, we’ll want to use GHC
  (setq agda2-backend "GHC")
  ;; the info window for Agda seems a little small, this is to encourage it to be taller
  (setq agda2-information-window-max-height 0.5)
  ;; Activate agda-mode for regular and literate (markdown) Agda files
  :mode (("\\.agda\\'"     . agda2-mode)
         ("\\.lagda.md\\'" . agda2-mode)))

;; Agda's neat input method runs on the quail minor mode (included in Emacs,
;; originally from AIST Japan). Ensure it's loaded in the same way as other packages
;; (Agda-input just loads via a `require`)
(use-package quail
  :straight (:type built-in)
  :demand t)

(use-package agda-input
  :straight (:type git :host github :repo "paulroden/agda-mode")
  :demand t
  :after quail
  :custom (default-input-method "Agda"))

(use-package idris2-mode
  :straight (:type git :host github :repo "idris-community/idris2-mode"))

(use-package haskell-mode
  :straight t
  :custom
  (haskell-compile-cabal-build-command
   "cabal build all -ferror-spans -funclutter-valid-hole-fits")
  :config
  (defcustom haskell-formatter 'ormolu
    "The Haskell formatter to use. One of: 'ormolu, 'stylish, nil. Set it per-project in .dir-locals."
    :safe 'symbolp)

  (defun haskell-smart-format ()
    "Format a buffer based on the value of 'haskell-formatter'."
    (interactive)
    (cl-ecase haskell-formatter
      ('ormolu (ormolu-format-buffer))
      ('stylish (haskell-mode-stylish-buffer))
      (nil nil)
      ))

  (defun haskell-switch-formatters ()
    "Switch from ormolu to stylish-haskell, or vice versa."
    (interactive)
    (setq haskell-formatter
          (cl-ecase haskell-formatter
            ('ormolu 'stylish)
            ('stylish 'ormolu)
            (nil nil))))

  :bind (:map haskell-mode-map
         ("C-c a c" . haskell-cabal-visit-file)
         ("C-c a i" . haskell-navigate-imports)
         ("C-c m"   . haskell-compile)
         ("C-c a I" . haskell-navigate-imports-return)
         :map haskell-cabal-mode-map
         ("C-c m"   . haskell-compile)))

(use-package haskell-snippets
  :straight t
  :after (haskell-mode yasnippet)
  :defer)

;; s trings
(use-package s
  :straight t)
;; - nicer maps and so on
(use-package dash
  :straight t)

(use-package geiser-chez
  :straight t
  ;; geiser tries to run `scheme' unless informed otherwise
  :custom (geiser-chez-binary "chez"))
(use-package geiser-racket
  :straight t)

(use-package geiser-mit
  :straight t
  :disabled t)
(use-package geiser-chibi
  :straight t
  :disabled t)

(use-package racket-mode
  :straight t)

(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t)

(use-package zmq
  :straight t)

(use-package jupyter
  :straight t)

(use-package elpy
  :straight t
  :init
  (setq python-indent-offset 4)
  (advice-add 'python-mode :before 'elpy-enable))

(use-package go-mode
  :straight t)

(use-package rust-mode
  :custom
  (lsp-rust-server 'rust-analyzer))

(use-package rustic
  :straight t
  :blackout " 🦀"
  :bind (:map rustic-mode-map
              ("C-c a t" . rustic-cargo-current-test)
              ("C-c m" . rustic-compile)
              ("C-c C-c l" . flycheck-list-errors))
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  :config
  ;; flymake is not necessary here; disable it
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

;; also cargo-mode offers a more ergonomic Emacs interface to cargo
(use-package cargo-mode
  :straight t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; Since the modern web = JavaScript this goes here
(use-package web-mode
  :straight t
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))
;; ... or should that be, the modern web === 'JavaScript' ??

;; JavaScript bits
(use-package js2-mode
  :straight t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq-default js-indent-level 2))

(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :custom (typescript-indent-level 2))

(use-package rjsx-mode
  :straight t
  :mode ("components\\/.*\\.js\\'" . rjsx-mode))

(use-package tide
  :straight t
  :after (typescript-mode flycheck corfu)
  :config
  ;; flycheck integration
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; tree-sitter highlighting is nice for JSX syntax
         (typescript-mode . tree-sitter-hl-mode)
         (before-save . tide-format-before-save)))

(straight-use-package 'markdown-mode)

(straight-use-package 'graphviz-dot-mode)

(use-package dhall-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package toml-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package nix-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))
  ;; (add-to-list 'eglot-server-programs
  ;;             '(nix-mode . ("rnix-lsp"))))

(use-package fish-mode
  :straight t
  :blackout "􁖐︎")

(use-package sh-mode
  :straight (:type built-in)
  :mode
  (("bashrc$" . sh-mode)
   ("zshrc$" . sh-mode)))

(use-package hideshow
  :straight (:type built-in))

(use-package sgml-mode
  :straight (:type built-in))

(use-package nxml-mode
  :straight (:type built-in)
  :config
  ;; Theme for tag-oriented markup
   (custom-theme-set-faces
   'user
   `(nxml-element-local-name
     ((t (:foreground ,(local/theme/colour 'fg-midi-mild)
          :weight light))))
   `(nxml-tag-delimiter
     ((t (:foreground ,(local/theme/colour 'fg-more-mild)
          :weight light)))))
   ;; hideshow config for tree folding
   (add-to-list 'hs-special-modes-alist
            '(nxml-mode
              "<!--\\|<[^/>]*[^/]>"
              "<!--\\|</[^>]*[^/]>"
              "<!--"
              sgml-skip-tag-forward
              nil))
   :hook
   (nxml-mode . hs-minor-mode))

(use-package titlecase
  :straight t)

(use-package electric
    :straight (:type built-in)
    :blackout (electric-quote-mode . " “„")  ;; ← not working
    :bind ("C-c q" . electric-quote-mode))
;;  (global-set-key (kbd "C-c q") 'electric-quote-mode)

(use-package markdown-mode
  :straight t
  :mode
  (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :blackout ((markdown-mode . "􀣿")
             (gfm-mode      . "􀤁"))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; Settings for visual convenience — note that these can be toggled, try `M-x ~whatever'
  (setq markdown-hide-urls t
        markdown-url-compose-char "↗"
        markdown-hide-markup t
        markdown-fontify-code-blocks-natively t
        markdown-inline-code-face t)
  ;; Some tweaks to pretty faces used in Markdown mode
   (custom-theme-set-faces
    'user
    '(markdown-inline-code-face      ((t (:height 0.9 :inherit fixed-pitch))))
    '(markdown-code-face             ((t (:height 0.9 :inherit fixed-pitch))))
    '(markdown-language-keyword-face ((t (:height 0.9 :inherit fixed-pitch)))))

  :hook
  ((markdown-mode . variable-pitch-mode)
   (markdown-mode . rainbow-mode)))

(use-package markdown-preview-mode
  :straight t)

(straight-use-package '(tex :type built-in))
(straight-use-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(use-package org
  :straight '(org :type built-in)
  :blackout "􀂱"
  :config
  (custom-theme-set-faces
   'user
   ;; Typefaces and fonts for the Org
   ;;   ... I don't know why Org Mode doesn't inherit my preferred fonts from
   ;;   the earlier config, so set the basic fixed-pitch face here.
   ;; setting this as a scale factor of 1.0 seems to permit 'zooming'
   `(fixed-pitch ((t (:font ,(local/fixed-width-font-family) :height 1.0))))
   ;;   set this one here for the same reason
   `(variable-pitch ((t (:font "Cormorant Infant"
                               :height 1.25
                               :foreground ,(local/theme/colour 'fg-deep)
                               :inherit default))))
   ;; Org-mode specific
   ;;   Default & general faces
   '(org-default ((t (:inherit variable-pitch))))
   '(org-link    ((t (:foreground "#001e38" :underline t))))
   '(org-indent  ((t (:inherit (org-hide fixed-pitch)))))
   ;;   Code-blocks and similar
   '(org-code             ((t (:height 0.8 :inherit fixed-pitch))))
   '(org-verbatim         ((t (:height 0.8 :inherit fixed-pitch))))
   '(org-block            ((t (:background "#f7f5f5" :height 0.8 :inherit fixed-pitch))))
   '(org-block-begin-line ((t (:background "#f2f0f0" :height 0.8 :inherit fixed-pitch))))
   '(org-block-end-line   ((t (:background "#f2f0f0" :height 0.8 :inherit fixed-pitch))))
   ;;   Headings
   '(org-document-title ((t (:font "Playfair Display" :height 1.50 :weight semi-light))))
   '(org-level-1        ((t (:font "Playfair Display" :height 1.35 :weight extra-bold))))
   '(org-level-2        ((t (:font "Playfair Display" :height 1.25 :weight semi-bold ))))
   '(org-level-3        ((t (:font "Playfair Display" :height 1.20 :weight bold      ))))
   '(org-level-4        ((t (:font "Playfair Display" :height 1.15 :weight semi-bold ))))
   '(org-level-5        ((t (:font "Playfair Display" :height 1.10 :weight bold      ))))
   '(org-level-6        ((t (:font "Playfair Display" :height 1.05 :weight semi-bold ))))
   '(org-level-7        ((t (:font "Playfair Display" :height 1.05 :weight bold      ))))
   '(org-level-8        ((t (:font "Playfair Display" :height 1.02 :weight bold      ))))
   ;;   org-mode metadata
   '(org-document-info         ((t (:slant italic :weight semi-bold))))
   `(org-document-info-keyword ((t (:foreground ,(local/theme/colour 'fg-mild) :slant italic :weight semi-light))))
   '(org-meta-line             ((t (:inherit org-document-info-keyword))))
   '(org-property-value        ((t (:inherit fixed-pitch))) t))

  ;; other tweaks to Org mode
  (setq org-startup-indented t
        org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        ;; folding symbol
        org-ellipsis "  "
        org-pretty-entities t
        ;; Maybe we don't want to see the style, not the things which make the style (let's see how this goes)
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        ;; use org-babel to decorate code blocks
        org-src-fontify-natively t
        ;; and tab-complete like you'd expect
        org-src-tab-acts-natively t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)

  :bind
  (("C-c l" . #'org-store-link)
   ("C-c a" . #'org-agenda)
   ("C-c c" . #'org-capture))

  :hook
  ((org-mode . org-indent-mode)
   ;; And enable variable-pitch when in Org Mode, for all those typographic tweaks above
   (org-mode . variable-pitch-mode)
   ;; And Rainbow mode is often desirable in Org., such as when editing this very file.
   (org-mode . rainbow-mode)))

(use-package org-indent
  :straight '(org :type built-in)
  :blackout " 􀋵")

(use-package org-contrib
  :straight t)

;; Fire the default bullets
(use-package org-bullets
  :straight t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Something for prettier tables
(use-package org-pretty-table
  :straight (:type git :host github :repo "Fuco1/org-pretty-table")
  :blackout " 􀏣"
  :hook (org-mode . (lambda () (org-pretty-table-mode))))

(use-package ob             :straight (:type built-in))
(use-package ob-eval        :straight (:type built-in))
(use-package ob-scheme      :straight (:type built-in))
(use-package ob-emacs-lisp  :straight (:type built-in))
(use-package ob-lisp        :straight (:type built-in))
(use-package ob-haskell     :straight (:type built-in))
(use-package ob-python      :straight (:type built-in))
(use-package ob-jupyter     :straight (:type built-in))
;; 🧜🏽‍♀️ https://emacstil.com/til/2021/09/19/org-mermaid/
(use-package ob-mermaid     :straight t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python       . t)
   (emacs-lisp   . t)
   (scheme       . t)
   (haskell      . t)
   (mermaid      . t)))
