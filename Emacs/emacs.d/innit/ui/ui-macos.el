;;; ui-macos.el --- UI Elements when on MacOS  -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:
(require 'innit-useful)

;; The world's best trackpads do not have a middle-click function.
(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
(define-key key-translation-map (kbd "<s-down-mouse-1>") (kbd "<down-mouse-2>"))


;; Clean up chrome (maybe not only MacOS)
;; (when (window-system)
;;   (progn
;;     (tool-bar-mode -1)
;;     (scroll-bar-mode -1)))
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Show a little proxy item in the title bar.
(setq ns-use-proxy-icon t)

;; Allow transparency in the titlebar
(setq default-frame-alist
      (append default-frame-alist
              '((ns-transparent-titlebar . t)
                (ns-appearance . dark))))

;; Default frame dimensions
;; TODO: use known display size defaults when not in `window-system'
(when window-system
  (setq default-frame-alist
        (append default-frame-alist
  	      (append
  		 ;; derive width and height from display size with the below factors
  		 (let ((scale-factors
  			`((width . ,(* 3 (frame-char-width)))
  			  (height . ,(* 3 (frame-char-height))))))
  		   (scale-keys #'/ scale-factors (or (get-mouse-pointer-display-dimensions)
  						     (get-display-dimensions))))
  		 ;; derive offset from top left as 10% of display
  		 (rename-keys
  		  '((width . left)
  		    (height . top))
  		  (scale-keys #'/ 10 (or (get-mouse-pointer-display-dimensions)
  					 (get-display-dimensions))))))))

(use-package ns-auto-titlebar
  :if (memq window-system '(mac ns))
  :straight t
  :config
  (ns-auto-titlebar-mode))

;;; Mac Modifier Keys
(when (equal system-type 'darwin)
  ;; soon we'll need to bind the Apple Ultra key, no doubt...
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier  'meta)
  (setq ns-function-modifier 'hyper))

;;; Bind fn-E (a.k.a. 􀆪 E) to insert character palette
(global-set-key (kbd "H-e") #'insert-char)

(provide 'ui-macos)
;;; ui-macos.el ends here.
