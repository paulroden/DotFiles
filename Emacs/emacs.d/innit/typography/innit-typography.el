;;; innit-typography.el --- Providing the riff for your serif  ; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; 
;;; Code:


(defconst font--fixed-width-family
  "MonoLisa"
  "The font family used for fixed-with circumstances.
Unless otherwise stated.")

(set-fontset-font t 'symbol
                  (font-spec :family font--fixed-width-family))
(set-fontset-font t 'symbol
                  (font-spec :family "Apple Color Emoji") nil 'append)
(set-fontset-font t 'symbol
                  (font-spec :family "SF Mono") nil 'append)

;; More pretty items via SF Symbols 􀪱, believed to be in Unicode range [u00100000,u00101741]
(set-fontset-font t '(#x100000 . #x101800) "SF Compact")

(set-face-attribute 'default nil
		    :family font--fixed-width-family
		    :height 140
		    :inherit 'default)

(set-face-attribute 'fixed-pitch nil
		    :family font--fixed-width-family
		    :height 140
		    :inherit 'default)


;; Make the line numbers match the system aesthetic (TODO: is this UI? what about Linux?)
;; see: ui/ui-framing.el
(set-face-attribute 'line-number nil
                    :font "SF Mono"
                    :weight 'light
                    :foreground "grey")
(set-face-attribute 'line-number-current-line nil
                    :font "SF Mono"
                    :weight 'medium
                    :foreground "black")

(set-face-attribute 'minibuffer-prompt nil
                    :font "SF Mono"
                    :weight 'light)


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
  :hook (haskell-mode . ligature-mode))


(provide 'innit-typography)
;;; innit-typography.el ends here.
