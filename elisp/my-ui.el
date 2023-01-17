;; Remove unnecessary bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(setq inhibit-startup-message t)     ; no startup message
(line-number-mode t)                 ; line number in modeline
(column-number-mode t)               ; column number in modeline
(global-display-line-numbers-mode 1) ; Turn this on for line numbers
(transient-mark-mode t)              ; highlight the active region

(setq scroll-margin 0                    ; how far from top to start scrolling
      scroll-preserve-screen-position 1  ; keep point at screen position when scrolling
      scroll-step 1                      ; number of lines to scroll
      scroll-conservatively 10000)       ; scrolls up to 10000 lines to bring point barely on screen

(show-paren-mode 1)
(setq show-paren-delay 0             ; always show parens immediately
      show-paren-style 'expression)  ; highlight expression enclosed by parens

(require 'uniquify)                        ; files with same base name will be disambiguated
(setq uniquify-buffer-name-style 'forward) ; use forward slashes for names

(global-font-lock-mode t) ; always syntax highlight
(global-hl-line-mode t)   ; always highlight current line
(setq visible-bell t) ; use a visible bell
(setq ring-bell-function 'ignore) ; call 'ignore when bell would ring

;; Pretty symbols
;(global-prettify-symbols-mode t)

;; UTF 8
(setq local-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (member "Fira Code" (font-family-list))
  (set-face-font 'default (font-spec :family "Fira Code" :size 28)))

(setq widget-image-enable nil)

;; Cursor
(setq-default cursor-type 'box)
(blink-cursor-mode 1)

(global-subword-mode 1)

;; Margins and stuff
(fringe-mode '(0 . 0))


(set-window-margins nil 0 0) ; what are margins for?
(setq-default fill-column 80) ; wraparound column

(provide 'my-ui)
