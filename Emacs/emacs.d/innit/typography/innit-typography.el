;;; innit-typography.el --- Providing the riff for your serif  ; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:

(defconst font--fixed-width-family
  "JuliaMono"
  "The font family used for fixed-with circumstances. Unless otherwise stated.")

(use-package emacs
  :init
  ;;  enable display of Apple's SF Symbols. e.g. 􀪱, believed to be in Unicode range [u00100000,u00101741]
  (set-fontset-font t '(#x100000 . #x101800) "SF Compact")
  ;; (setup-font-for-sf-symbols)
  :config
  (setq-default line-spacing 4) ;; pleasant when using Julia Mono, among others
  (set-fontset-font t 'symbol
                    (font-spec :family font--fixed-width-family))
  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Color Emoji") nil 'append)
  (set-fontset-font t 'symbol
                    (font-spec :family "SF Mono") nil 'append)

  (set-face-attribute 'default nil
		      :family font--fixed-width-family
		      :height 140
		      :inherit 'default)

  (set-face-attribute 'fixed-pitch nil
		      :family font--fixed-width-family
		      :height 140
		      :inherit 'default)

  ;; Make the line numbers match the system aesthetic
  ;; see: ui/ui-framing.el
  (set-face-attribute 'line-number nil
                      :font "SF Mono"
                      :weight 'light
                      :foreground "#d3d1c9")
  (set-face-attribute 'line-number-current-line nil
                      :font "SF Mono"
                      :weight 'medium
                      :foreground "#48463d")
  (set-face-attribute 'minibuffer-prompt nil
                      :font "SF Mono"
                      :weight 'light))


;;; Ligatures
(use-package ligature
  :straight t
  :config
  (ligature-set-ligatures
   '(text-mode org-mode)
   '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   '(html-mode nxml-mode web-mode)
   '("<!--" "-->" "</>" "</" "/>" "://"))
  (ligature-set-ligatures
   'prog-mode
   '("www"   "**"    "***"   "**/"   "*>"    "*/"    "\\\\"  "---"
     "{-"    "-}"    "{|"    "|}"    "[|"    "|]"    "'''"   "\"\"\""
     "::"    ":::"   ":="    "::="   "!!"    "=="    "==="   "!="
     "!=="   "=!="   "~="    "..="   "=:="   "#="    "----"  "-->"
     "->"    "->>"   "-<"    "--<"   "-<<"   "<-<"   ">->"   "-~"
     "<~>"   "#{"    "#["    "]#"    "##"    "###"   "####"  "#("
     "#?"    "#_"    "#_("   ".-"    ".="    ".."    "..<"   "..."
     ".?"    "?="    "??"    ";;"    "/*"    "/**"   "/="    "/=="
     "/>"    "//"    "///"   "||"    "||="   "|="    "=/"           ;; "|>"
     "^="    "$>"    "++"    "+++"   "+>"    "==>"   "=>"    "=>>"
     "<==>"  "<="    "=<<"   "=/="   ">-"    ">="    ">=>"   ">>"
     ">>-"   ">>="   ">>>"   "<*"    "<*>"   "|||"           "<||"  ;; "<|"
     "<|||"          "<$"    "<$>"   "<!--"  "<-"    "<--"   "<->"  ;; "<|>"
     "<+"    "<+>"   "<="    "<=="   "<=>"   "<=<"   "<>"    "<<"
     "<<-"   "<<="   "<<<"   "<~"    "<~~"   "</"    "</>"   "<:"
     ":<"    ":>"    "~@"    "~-"    "~>"    "~~"    "~~>"   "%%"
     "&&"    "!!."   "!!!"
     ))
  (global-ligature-mode t)
  :hook (prog-mode . ligature-mode))

;; Useful tool for debugging font lock issues
;; https://github.com/Lindydancer/font-lock-studio
(use-package font-lock-studio
  :straight t)

(provide 'innit-typography)
;;; innit-typography.el ends here.
