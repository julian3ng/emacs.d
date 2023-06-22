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

(setq set-mark-command-repeat-pop t)

(setq scroll-margin 0                   ; how far from top to start scrolling
      scroll-preserve-screen-position 1 ; keep point at screen position when scrolling
      scroll-step 1                     ; number of lines to scroll
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
(setq blink-cursor-blinks 0) ; blink forever


(global-subword-mode 1)

;; Margins and stuff
(fringe-mode 8)
;; (set-face-foreground 'fringe "cyan")


;; (set-window-margins nil 0 0) ; what are margins for?
(setq-default fill-column 80) ; wraparound column

;; (setq-default frame-title-format
;;               '(
;;                 "<"
;;                 (eyebrowse-mode (:eval (number-to-string (eyebrowse--get 'current-slot))))
;;                 ">"
;;                 " "
;;                 (eyebrowse-mode (:eval (eyebrowse-mode-line-indicator)))))

(setq-default frame-title-format '((projectile-mode projectile-project-name)))

(setq-default header-line-format
              '((buffer-file-name (:eval (abbreviate-file-name buffer-file-name)))
                (dired-directory dired-directory)))

(setq-default  mode-line-format
               '("%e"
                 mode-line-front-space
                 mode-line-mule-info
                 mode-line-client
                 mode-line-modified
                 mode-line-remote
                 mode-line-frame-identification
                 mode-line-buffer-identification
                 " "
                 mode-line-position
                 " "
                 mode-line-modes
                 mode-line-misc-info
                 mode-line-end-spaces))

(add-hook 'prog-mode-hook 'hs-minor-mode)

(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

(when (>= emacs-major-version 29)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq sentence-end-double-space nil)
(whitespace-mode t)
(setq-default truncate-lines nil)

(defun julian/ui-flash-fringe ()
  "Flash the fringe on error or warning instead of the bell."
  (invert-face 'fringe)
  (run-with-timer 0.1 nil #'invert-face 'fringe))

(setq ring-bell-function #'julian/ui-flash-fringe)

(setq isearch-lazy-count t)

;; Window display management: look at
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun julian/toggle-window-dedication ()
  "Toggles window dedication in selected window"
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(setq window-sides-slots '(1 1 1 1))

;; switch to buffer acts like programmatic switch to buffer
(setq switch-to-buffer-obey-display-actions t)

(setq switch-to-buffer-in-dedicated-window 'pop)

;; Custom buffer display behavior
(setq display-buffer-alist '(
                             ;; Helpful buffers stay in one window
                             ("\\*helpful.*\\*" (display-buffer-reuse-mode-window))
                             ;; Magit buffers stay in one window
                             ("magit.*" (display-buffer-reuse-mode-window) (mode . (magit-mode magit-log-mode)))))

(buffer-match-p "\\*helpful.*\\*" "*helpful variable: display-buffer-alist*")

(setq-default tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab))


;; Fix frame stuff
;;(set-frame-parameter (selected-frame) 'window-state nil)


(provide 'my-ui)
