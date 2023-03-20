;;; workspace-projects.el --- Projecting projects -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   
;;; Code:

(require 'recentf)

(use-package project
  :demand t
  :functions project-root
  :bind (("C-c k" . #'project-kill-buffers)
         ("C-c m" . #'project-compile)
         ("C-x f" . #'find-file)  ; cf C-x C-f, but more in line with the others here
         ("C-c f" . #'project-find-file)
         ("C-c F" . #'project-switch-project)
         ("C-c R" . #'recentf-in-project))
  :custom
  (project-switch-commands
   '((project-find-file "Find File")
     (magit-project-status "Magit" ?g)
     (deadgrep "Grep" ?h)
     (vterm-for-project ?d)
     (project-dired "Dired" ?d)
     (recentf-in-project "Recently Opened" ?r)))
  (compilation-always-kill t)
  (project--vc-merge-submodules nil)
  ;; And include the helper function above as a hook.
  ;; Note that we cannot use :hook here because `project-find-functions' doesn't
  ;; end in "-hook", and we can't use this in :init because it won't be defined
  ;; yet.
  :config
  (add-hook 'project-find-functions #'project-root-override))

(defun recentf-in-project ()
  "As `recentf', but filtered based on the current project root, credit @patrickt."
  (interactive)
  (let* ((proj (project-current))
         (root (if proj (project-root proj) (user-error "Not currenly in a project"))))
    (cl-flet ((ok (fpath) (string-prefix-p root fpath)))
      (find-file (completing-read "Find recent file:" recentf-list #'ok)))))
         
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

                   
(provide 'workspace-projects)
;;; workspace-projects.el ends here
