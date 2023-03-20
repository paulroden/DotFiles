;;; environment-codings.el --- Coding systems and their settings  ;;; -*- lexical-binding: t -*-
;;; Commentary:
;;;   This is mostly about Unicode / UTF-8 input
;;; Code:

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


(provide 'environment-codings)
;;; environment-codings.el ends here.
