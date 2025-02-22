;; Taken from https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/early-init.el
;; and https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el
;; Increase garbage collection threshold to speed up initialization
(setq julian--original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 100 1000 1000))
(setq byte-compile-warnings '(not obselete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq inhibit-startup-screen t
      initial-buffer-choice nil
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-startup-echo-area-message (user-login-name)
      inhibit-splash-screen t)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
;; Give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

;; Startup time optimization
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq frame-resize-pixelwise t)
(setq tool-bar-mode -1)

(setq frame-inhibit-implied-resize t)
(setq auto-mode-case-fold nil)

(setq use-package-enable-imenu-support t)

(when (and (featurep 'native-compile)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq
   native-comp-jit-compilation t
   native-comp-async-report-warnings-errors 'silent
   package-native-compile t))

(setq user-full-name "Julian Eng"
      user-mail-address "julian3ng@gmail.com")

(setq default-frame-alist '((fullscreen . maximized)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
