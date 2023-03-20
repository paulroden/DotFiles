;;; workspace-search.el --- Anything Finds Things  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

;; ripgrep (https://github.com/BurntSushi/ripgrep) via deadgrep https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :straight t
  :bind* (("<f5>" .  #'deadgrep)
          ("H-s-f" . #'deadgrep)))

(use-package anzu
  :straight t
  :blackout " 杏"
  :bind (("M-%" . #'anzu-query-replace)
         ("C-M-%" . #'anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; `list-matching-lines' is /so/ useful
(global-set-key (kbd "C-s-f") #'list-matching-lines)


(provide 'workspace-search)
;;; workspace-search.el ends here.
