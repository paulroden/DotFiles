;;; environment-behaviour.el --- Behaviours of 'Innit'  -*- coding: utf-8; lexical-binding: t -*-
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; 
;;; Code:
;;;

;; Startup settings (see `startup.el')
(setq
 ;; No need to see GNU agitprop; we’ve already decided to use Emacs.
 inhibit-startup-screen t
 ;; Scratch buffers start as a clean, blank sheet.
 initial-scratch-message nil
 ;; Start in a clean scratch buffer as the default,
 ;; or whichever file or directory has been passed to `emacs'
 initial-buffer-choice nil)


;; General preferred behaviours
(setq
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
 ;; accept 'y' or 'n' instead of yes/no because too much typing
 use-short-answers t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; unicode ellipses are better
 truncate-string-ellipsis "…")



;; External files, directories and saving
;; (these need to be defined at startup)
;; Default working directory - this is most often where we'll want to begin
(setq default-directory "~/Projects/")

;; And we never want to have transient, state-dependent config. Custom configs
;; are be sent into the aether, never to be found again. Best not to have to
;; recall every random keypress or innebriated package install.
(setq custom-file (make-temp-name "/tmp/"))

;; More reduction of filesystem clutter that does not befit modern computing.
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; Save what we've done
(use-package savehist
  :init (savehist-mode))
;; Also, remember to save where we were...
(save-place-mode 1)

;; Remember what's happened recently
(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :init
  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items 50)
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :hook (dired-mode . recentf-add-dired-directory)
  :config
  ;; exclude directories of files which are frequently visited in the background
  (add-to-list 'recentf-exclude
               '("\\elpa"
                 "\\straight"
                 "private/tmp"))
  ;; Periodically save recent files, every 5 minutes
  (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; …and that means once every 5 minutes for each file that is being visited
  ;; and is saveable, so quiescing "wrote ... recentf" messages is sane.
  ;; this SO post has a solution [https://emacs.stackexchange.com/a/68323]
  (defun suppress-msg (function)
    "Prevent FUNCTION from showing `Wrote <FILE>' messages.
      (The messages are still logged to `*Messages*'.)"
    (let ((inhibit-message t))
      (funcall function)))
  
  (advice-add 'recentf-save-list :around 'suppress-msg)
  (recentf-mode +1))

;; Allow us to log all the commmands
;; Show command log buffer with `clm/open-command-log-buffer'
;; See also, `c-h l' to view lossage
(use-package command-log-mode
  :straight t
  :demand t)


;; File time stamps
;; This enables time stamps to be inserted in files and updated on save.
;; Given either of the two forms of text in the first 8 lines of a file:
;;     Time-stamp: <>
;;     Time-stamp: " "
;; Emacs will fill this with a time stamp defined by `time-stamp-pattern`
;; For details, see https://www.emacswiki.org/emacs/TimeStamp
(add-hook 'before-save-hook 'time-stamp)


(provide 'environment-behaviour)
;;; environment-behaviour.el ends here.
