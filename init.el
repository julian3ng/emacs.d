(setq julian/at-work (eq system-type 'darwin))
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t
        use-package-always-ensure t))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Setup packages
(require 'package)
                                        ;(setq package-enable-at-startup nil)

;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities '(("melpa"    . 5)
                                   ("jcs-elpa" . 0)))


(setq load-prefer-newer t)

(put 'upcase-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/elisp/")


(add-to-list 'load-path "~/.emacs.d/pico8-mode/")
(when (file-exists-p "~/.emacs.d/pico8-mode/")
  (require 'pico8-mode))

(require 'my-system)
(require 'my-keybinds)
(require 'my-packages)
(require 'my-ui)
(require 'my-skeletons)


(tab-bar-mode t)

(if (not (require 'o4m-utils nil t))
    (message "'o4m-utils not found"))
;;(require 'linear)
;;(require 'my-meow)



;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-vc-selected-packages '((aider :url "https://github.com/tninja/aider.el"))))


;; Reset garbage collection threshold
(setq gc-cons-threshold julian--original-gc-cons-threshold)
(put 'overwrite-mode 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defmacro delay (&rest body)
  `(lambda () (progn ,@body)))

(defmacro force (thunk)
  `(funcall ,thunk))

(defun julian/ensure-tab (position command group)
  (if (<= position (length (tab-bar-tabs)))
      (tab-bar-select-tab position)
    (tab-bar-new-tab-to position))
  (tab-bar-change-tab-group group position)
  (funcall command))

(defun julian/setup-workspaces ()
  (tab-bar-close-other-tabs 1)
  (julian/ensure-tab 1 #'elfeed-summary "ETC")
  (julian/ensure-tab 2 #'ielm "ETC")
  (julian/ensure-tab 3 (delay (find-file "~/Documents/Outcomes4me/api/") (magit-status)) "API")
  (julian/ensure-tab 4 (delay (find-file "~/Documents/Outcomes4me/mobile-3/") (magit-status)) "MOBILE")
  (julian/ensure-tab 5 (delay (find-file "~/Documents/Outcomes4me/web") (magit-status)) "WEB")
  (julian/ensure-tab 6 (delay (find-file "~/Documents/Outcomes4me/infrastructure") (magit-status)) "INFRA"))

(when julian/at-work
  (julian/setup-workspaces))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)

;; Not sure where to put this so that it's always evaluated?
(define-advice sly-completing-read (:around (fn &rest args))
  (let ((icomplete-mode t))
    (apply fn args)))

;; TODO: figure out how to configure ff-find-other-file and similar
