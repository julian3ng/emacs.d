(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-function-modifier 'hyper)
(setq mac-pass-command-to-system nil)

(global-set-key [mouse-3] 'xref-go-back)
(global-set-key [mouse-4] 'xref-find-definitions)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-`") 'other-frame)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-M-t") 'transpose-regions)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-c C-r") 'rgrep )
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z b") 'bury-buffer)
(global-set-key (kbd "C-z u") 'unbury-buffer)
(global-set-key (kbd "<f2>") 'follow-mode)
(global-set-key (kbd "M-O") 'other-frame)
(unbind-key (kbd "s-a"))
(unbind-key (kbd "s-h"))
(unbind-key (kbd "s-l"))
(unbind-key (kbd "s-o"))
(unbind-key (kbd "s-p"))
(unbind-key (kbd "s-t"))
(unbind-key (kbd "s-e"))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun julian/copy-location-and-basename ()
  (interactive)
  (let ((locstring
         (concat
          (if (buffer-file-name)
              (file-relative-name (buffer-file-name))
            (buffer-name))
          ":"
          (number-to-string (1+ (current-line))))))
    (kill-new locstring)
    (message locstring)))


(unbind-key (kbd "s-j"))
(global-set-key (kbd "s-j g") 'julian/copy-location-and-basename)


(defun julian/set-font-size (prefix)
  (interactive "p")
  (message "%d" prefix)
  (if (= prefix 1)
      (set-face-font 'default (font-spec :family "Fira Code" :size 12))
    (set-face-font 'default (font-spec :family "Fira Code" :size 10))))

(global-set-key (kbd "s-j f") 'julian/set-font-size)


(defun julian/toggle-window-dedication ()
  "Toggles window dedication in selected window"
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(global-set-key (kbd "s-j s") 'julian/toggle-window-dedication)


(global-set-key (kbd "C-x |") 'toggle-window-split)
(repeat-mode 1)


(defalias 'julian/setup-environment
  (kmacro "C-x t 2 s-p p a p i <return> C-x t 2 s-p p m o b i l e <return>"))

(defun julian/snake-to-camelcase (str)
  "Convert snakecase string to camelcase. i.e. foo_bar_123 to fooBar123"
  (replace-regexp-in-string
   "_."
   (lambda (match)
     (capitalize (substring match 1)))
   str))

(defun julian/snake-to-camelcase-region (start end)
  (interactive "r")
  (replace-region-contents start end (lambda () (julian/snake-to-camelcase (buffer-string)))))

(defun julian/snake-to-camelcase-word ()
  (interactive)
  (let ((start (1- (search-forward-regexp "[[:word:]]")))
        (end (1- (search-forward-regexp "[^[:word:]_]"))))
    (julian/snake-to-camelcase-region start end)))


(global-set-key (kbd "M-C") 'julian/snake-to-camelcase-word)

(defun julian/camel-to-snakecase (str)
  "Convert camelcase string to snakecase. i.e. fooBar123 to foo_bar123"
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "[a-z][A-Z]"
     (lambda (match)
       (concat (substring match 0 1)
               "_"
               (downcase (substring match 1))))
     str)))

(defun julian/camel-to-snakecase-region (start end)
  (interactive "r")
  (replace-region-contents start end (lambda () (julian/camel-to-snakecase (buffer-string)))))

(defun julian/camel-to-snakecase-word ()
  (interactive)
  (let ((start (1- (search-forward-regexp "[[:word:]]")))
        (end (1- (search-forward-regexp "[^[:word:]]"))))
    (julian/camel-to-snakecase-region start end)))

(global-set-key (kbd "M-L") 'julian/camel-to-snakecase-word)


;; ;;;; Example: writing your own keymap
;; (defvar-keymap test-prefix-buffer-map
;;   :doc "test buffer keymap"
;;   "s" #'save-buffer
;;   "w" #'write-buffer
;;   "p" #'previous-buffer
;;   "n" #'next-buffer)

;; (defvar-keymap test-prefix-map
;;   :doc "test prefix map"
;;   "b" test-prefix-buffer-map)

;; (which-key-add-keymap-based-replacements test-prefix-map
;;   "b" `("Buffer" . ,test-prefix-buffer-map))

;; (keymap-set global-map "s-t" test-prefix-map)

;; https://karthinks.com/software/emacs-window-management-almanac/#isearch-other-window
(defun isearch-other-window (regexp-p)
  "Function to isearch-forward in the next window.

With prefix arg REGEXP-P, perform a regular expression search."
  (interactive "P")
  (unless (one-window-p)
    (with-selected-window (other-window-for-scrolling)
      (isearch-forward regexp-p))))

(defun split-window-parent-below (arg)
  (interactive "p")
  (cond
   ((= arg 4) (split-window-below nil (window-parent)))
   ((= arg 16) (split-window-below nil (window-parent (window-parent))))
   (t (split-window-below nil nil))))

(defun split-window-parent-right (arg)
  (interactive "p")
  (cond
   ((= arg 4) (split-window-right nil (window-parent)))
   ((= arg 16) (split-window-right nil (window-parent (window-parent))))
   (t (split-window-right nil nil))))

(global-set-key (kbd "C-x 2") #'split-window-parent-below)
(global-set-key (kbd "C-x 3") #'split-window-parent-right)

(defun julian/test-propertize ()
  (interactive)
  (message (propertize "hello" 'face '(:foreground "red"))))



;; Fix (remove) treesit sexp commands
(defun julian/fix-treesit-sexp-commands ()
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function nil))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function nil)))

(add-hook 'prog-mode-hook #'julian/fix-treesit-sexp-commands)

(provide 'my-keybinds)
