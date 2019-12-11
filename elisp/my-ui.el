(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(setq inhibit-startup-message t)     ; no startup message
(line-number-mode t)                 ; line number in modeline
(column-number-mode t)               ; column number in modeline
(global-display-line-numbers-mode t) ; line numbers on left
(transient-mark-mode t)              ; highlight the active region

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
(set-face-background 'hl-line "#222") 

(setq ring-bell-function 'ignore) ; call 'ignore when bell would rinig

(setq fill-column 79) ; wraparound column

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

(provide 'my-ui)
