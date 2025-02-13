;; Taken from https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/early-init.el
;; Increase garbage collection threshold to speed up initialization
(setq julian--original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 100 1000 1000))
(setq byte-compile-warnings '(not obselete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))

(setq frame-resize-pixelwise t)
(setq tool-bar-mode -1)
