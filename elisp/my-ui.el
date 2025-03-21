;; Remove unnecessary bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(setq inhibit-startup-message t)     ; no startup message
(line-number-mode nil)
(column-number-mode nil)
(global-display-line-numbers-mode nil)
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
(defface julian/vibrant-face '((t (:background "#dd4444" :foreground "#ffffff" :underline t )))
  "Used for highlights")
(defface julian/blue-face '((t (:background "#00ffff" :foreground "#000000" :underline t))) "Used for highlights")

(defface julian/highlight '((t (:background "#444"))) "Used for highlights")

(defun julian/add-highlights ()
  (font-lock-add-keywords nil '(("TODO" 0 'julian/vibrant-face t)
                                ("NOTE" 0 'julian/blue-face t))))
(add-hook 'prog-mode-hook #'julian/add-highlights)

(custom-theme-set-faces
 'user
 '(hl-line ((t :underline (:color "lime" ) :background unspecified )))
 '(sideline-blame ((t
                    (:italic t :background unspecified :foreground "#7a88cf"))))
 ;; tab bar is for top level tabs 
 '(tab-bar-tab ((t :foreground "#66ff66" :background unspecified)))
 '(tab-bar-tab-inactive ((t :foreground "#7c6f64")))
 '(tab-bar-tab-group-current ((t :foreground "#282828"  :background "#ff8844")))
 '(tab-bar-tab-group-inactive ((t :foreground "#282828" :background "#446688" ))))

(setq tab-bar-separator " ⣿ ")
(setq tab-bar-tab-hints t)

(setq use-dialog-box nil)
(setq-default tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
(setq tab-bar-close-button-show nil
      tab-bar-button-relief 2
      tab-bar-auto-width t)

(setq tab-line-tabs-buffer-group-function 'tab-line-tabs-buffer-group-by-project)

(global-hl-line-mode t)   ; always highlight current line
(setq visible-bell t) ; use a visible bell
(setq ring-bell-function 'ignore) ; call 'ignore when bell would ring

;; UTF 8
(setq local-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq widget-image-enable nil)

;; Cursor
(setq-default cursor-type 'box)
(blink-cursor-mode -1)
(setq blink-cursor-blinks 0) ; blink forever


(global-subword-mode 1)

;; Margins and stuff
(fringe-mode 8)
(set-face-foreground 'fringe "cyan")


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
                 (:eval (when (window-dedicated-p (selected-window)) (propertize "!" 'font-lock-face '(:foreground "black" :background "red"))))
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
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq sentence-end-double-space nil)
;;(whitespace-mode t)
(setq-default truncate-lines nil)

(defun julian/ui-flash-fringe ()
  "Flash the fringe on error or warning instead of the bell."
  (invert-face 'fringe)
  (run-with-timer 0.1 nil #'invert-face 'fringe))

(setq ring-bell-function #'julian/ui-flash-fringe)

(setq isearch-lazy-count t)

(setq window-sides-slots '(1 1 1 1))

;; switch to buffer acts like programmatic switch to buffer
(setq switch-to-buffer-obey-display-actions t)

(setq switch-to-buffer-in-dedicated-window 'pop)

;; Custom buffer display behavior
(setq display-buffer-alist '(("\\*elfeed-.*\\*" . (display-buffer-full-frame))
                             ("\\*elfeed-.*\\*" . (display-buffer-reuse-window display-buffer-same-window))))
(setq display-buffer-reuse-frames t)         ; reuse windows in other frames
(setq even-window-sizes nil)                 ; display-buffer: avoid resizing


;; 'q' on a child frame won't do anything
(setq iconify-child-frame nil)

(set-face-attribute 'default nil :font "Fira Code" :height 100 :weight 'normal)


;; keep modeline output from going crazy
(setq eldoc-echo-area-use-multiline-p nil)

;; Fix frame stuff
;;(set-frame-parameter (selected-frame) 'window-state nil)
(which-function-mode t)
(setq-default which-func-display 'header)

(global-display-fill-column-indicator-mode)

(global-hi-lock-mode)
(setq shr-use-fonts nil)

(setq completions-detailed t)
(setq x-underline-at-descent-line t)
(setq display-line-numbers-width 3)

(which-key-mode 1)
(which-key-setup-side-window-right-bottom)
(setq which-key-idle-delay 0.5
      which-key-max-display-columns 1
      which-key-min-column-description-width 0
      which-key-add-column-padding 0
      which-key-max-description-length 50
      which-key-show-docstrings nil)

(setq bs-default-configuration "all-intern-last")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package autothemer)
(use-package modus-themes)
;; dark themes: ("kaolin-aurora" "kaolin-blossom"  "kaolin-bubblegum" "kaolin-dark" "kaolin-eclipse" "kaolin-galaxy" "kaolin-mono-dark" "kaolin-ocean" "kaolin-shiva" "kaolin-temple" "kaolin-valley-dark")

(use-package kaolin-themes)
(use-package gruvbox-theme)
(defun julian/load-theme-after-frame (&optional frame)
  (load-theme 'gruvbox-dark-medium t)
  (set-face-attribute 'mode-line-buffer-id nil :inherit nil))

(add-to-list 'after-make-frame-functions #'julian/load-theme-after-frame)
(julian/load-theme-after-frame)

;; These feel kinda slow
;; (use-package sideline-flymake :after sideline :config (setq sideline-flymake-display-mode 'line))
;; (use-package sideline-blame :after sideline)
;; (use-package sideline-eglot :after sideline)
;; (use-package sideline
;;   :config
;;   (setq sideline-backends-left '())
;;   (setq sideline-backends-right '(sideline-eglot sideline-blame))
;;   (global-sideline-mode t))

(use-package hideshow
  :ensure nil
  :hook ((prog-mode-hook . hs-minor-mode))
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("s-h a" . hs-show-all)
              ("s-h t" . hs-hide-all)
              ("s-h c" . hs-toggle-hiding)
              ("s-h d" . hs-hide-block)
              ("s-h l" . hs-hide-level)))

(use-package hideshowvis
  :hook ((prog-mode-hook . hideshowvis-enable)))

(use-package rainbow-mode :diminish rainbow-mode
  :hook ((prog-mode . rainbow-mode)))

(use-package rainbow-delimiters :diminish ""
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (ligature-set-ligatures 'org-mode '("www" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                      "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

;; ;; pulses modified regions
(use-package goggles
  :hook (prog-mode . goggles-mode)
  :config (setq-default goggles-pulse t))

(use-package focus)

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-global-mode))

(use-package writeroom-mode
  :hook (info-mode . writeroom-mode)
  :config (setq
           writeroom-fullscreen-effect 'maximized))

(use-package form-feed-st
  :config (global-form-feed-st-mode))


;; minibuffer
(setq resize-mini-windows t)
(minibuffer-depth-indicate-mode t)
(setq read-answer-short t)
(setq kill-ring-max 60)

(diminish global-auto-revert-mode)

(provide 'my-ui)
