;; Remove unnecessary bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(setq inhibit-startup-message t)     ; no startup message
(line-number-mode t)                 ; line number in modeline
(column-number-mode t)               ; column number in modeline
(global-display-line-numbers-mode 0) ; Turn this on for line numbers
(transient-mark-mode t)              ; highlight the active region

(set-face-background 'region "#355")

(setq scroll-margin 0                    ; how far from top to start scrolling
      scroll-preserve-screen-position 1  ; keep point at screen position when scrolling
      scroll-step 1                      ; number of lines to scroll
      scroll-conservatively 10000)       ; scrolls up to 10000 lines to bring point barely on screen

(show-paren-mode 1)
(setq show-paren-delay 0             ; always show parens immediately
      show-paren-style 'expression)  ; highlight expression enclosed by parens
(set-face-background 'show-paren-match-expression "#355")


(require 'uniquify)                        ; files with same base name will be disambiguated
(setq uniquify-buffer-name-style 'forward) ; use forward slashes for names

(global-font-lock-mode t) ; always syntax highlight
(global-hl-line-mode t)   ; always highlight current line
(set-face-background 'hl-line "#333") 

(setq visible-bell t) ; use a visible bell
(setq ring-bell-function 'ignore) ; call 'ignore when bell would ring

;; Comments
(set-face-foreground 'font-lock-comment-face "#888")
(set-face-foreground 'font-lock-comment-delimiter-face "#888")

;; Pretty symbols
(global-prettify-symbols-mode t)

;; UTF 8
(setq local-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Fonts

;; (when (member "JuliaMono" (font-family-list))
;;   (set-frame-font (font-spec :family "JuliaMono" :weight 'light :size 28)))
;; (when (member "Roboto Mono" (font-family-list))
;;   (set-frame-font  (font-spec :family "Roboto Mono" :weight 'light :size 28)))
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-frame-font  (font-spec :family "DejaVu Sans Mono" :weight 'light :size 28)))

(setq widget-image-enable nil)

;; Cursor
(setq-default cursor-type '(hbar . 4))
(blink-cursor-mode 0)

;; Margins and stuff
(fringe-mode '(0 . 0))
(set-window-margins nil 1 1)
(setq fill-column 79) ; wraparound column

(provide 'my-ui)
