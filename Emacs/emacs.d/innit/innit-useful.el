;;; innit-useful.el --- Common Functions for 'Innit' -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;;   "Common" in the sense that they are frequently used.
;;;   Load these early and use them often.
;;; Code:

(defconst innit-dir (file-name-directory load-file-name)
  "Path to the directory that contains `innit-environment'.")

;; Innit available on load-path
(add-to-list 'load-path innit-dir)
(add-to-list 'load-path (concat innit-dir "environment"))
(add-to-list 'load-path (concat innit-dir "ui"))
(add-to-list 'load-path (concat innit-dir "typography"))
(add-to-list 'load-path (concat innit-dir "workspace"))

;; System environment functions
(defun is-macos ()
  "Yields true if invoked on a MacOS machine."
  (eq system-type 'darwin))

(defun is-linux ()
  "Yields true if invoked on a Linux machine."
  (eq system-type 'gnu/linux))
					;
;; Provide useful function to locate `agda-mode' files whenever needed
(defun agda-mode-path ()
  "Provide the location of agda-mode elisp files.
This path is determined using `agda-mode's self-referental `locate' command
which is effecively ensured to work with Nix-based setup TODO: improve"
  (file-name-directory
    (shell-command-to-string "agda-mode locate")))


;; Frame & window utility functions
(defun scale-keys (op factors alist)
  "Apply operation OP to each value in ALIST by scalar(s) in FACTORS.
If FACTORS is a single value, multiply all values in ALIST by this scalar.
If FACTORS is an alist, apply OP with each correspoinding value in ALIST and FACTORS."
  (if (numberp factors)
      ;; If factors is a single number, use it as a scalar
      (mapcar (lambda (item)
                (cons (car item) (round (funcall op (cdr item) factors))))
              alist)
    ;; Otherwise, treat factors as an alist of scalars
    (mapcar (lambda (item)
              (let ((key (car item))
                    (value (cdr item))
                    (factor (cdr (assoc (car item) factors))))
                (cons key (round (funcall op value (or factor 1))))))
            alist)))

(defun rename-keys (name-map alist)
  "Rename keys in a ALIST, mapping any correspondning keys in NAME-MAP."
  (mapcar
   (lambda (item)
     (cons (alist-get (car item) name-map) (cdr item)))
   alist))

(defun find-display-by-pointer-position (x y)
  "Return the display containing the position (X, Y)."
  (seq-find (lambda (display)
              (let* ((geometry (alist-get 'geometry display))
                     (x-origin (nth 0 geometry))
                     (y-origin (nth 1 geometry))
                     (width (nth 2 geometry))
                     (height (nth 3 geometry)))
                (and (<= x-origin x (+ x-origin width))
                     (<= y-origin y (+ y-origin height)))))
            (display-monitor-attributes-list)))

(defun get-focused-frame-display-dimensions ()
  "Return an alist with the dimensions of the display that has the focused frame."
  (let* ((frame (selected-frame))
         (frame-pos (frame-position frame))
         (x (car frame-pos))
         (y (cdr frame-pos))
         (display (find-display-by-pointer-position x y)))
    (if display
        (let* ((geometry (alist-get 'geometry display))
               (width (nth 2 geometry))
               (height (nth 3 geometry)))
          `((width . ,width) (height . ,height)))
      (error "No display found for the focused frame"))))

(defun get-mouse-pointer-display-dimensions ()
  "Return an alist with the dimensions of the display that has the mouse pointer.
If no display is found, this returns nil."
  (let* ((mouse-pos (mouse-position))
         (frame (car mouse-pos))
         (x (cadr mouse-pos))
         (y (cddr mouse-pos))
         (display (find-display-by-pointer-position x y)))
    (if display
        (let ((geometry (alist-get 'geometry display)))
          `((width . ,(nth 2 geometry))
	    (height . ,(nth 3 geometry)))))))

(defun get-display-dimensions ()
  "Return an alist with the width and height of the first known display [pixels]."
  `((width . ,(display-pixel-width))
    (height . ,(display-pixel-height))))


(provide 'innit-useful)
;;; innit-useful.el ends here.
