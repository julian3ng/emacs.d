(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-function-modifier 'hyper)
(setq mac-pass-command-to-system nil)

;; C-<ARROW> moves in that direction
(windmove-default-keybindings 'control)

(setq tab-bar-select-tab-modifiers '(control))

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
(global-set-key (kbd "C-x C-b") 'bs-show)
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

(global-set-key (kbd "C-x |") 'toggle-window-split)
(repeat-mode 1)

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


(defun julian/delete-flymake-window ()
  (interactive)
  (let ((target (get-buffer-window (concat "*Flymake diagnostics for `" (buffer-name (current-buffer)) "'*") 'visible)))

    (if target
        (delete-window target)
      (message "No flymake window for this buffer"))))

(global-set-key (kbd "s-e k") 'julian/delete-flymake-window)

(setq-default forward-sexp-function 'forward-sexp-default-function)
(setq-default transpose-sexp-function 'transpose-sexp-default-function)
(setq-default forward-sentence-function 'forward-sentence-default-function)
;; Fix (remove) treesit sexp commands
(defun julian/fix-treesit-sexp-commands ()
  (when (and (boundp 'forward-sexp-function) (eq forward-sexp-function #'treesit-forward-sexp))
    (setq forward-sexp-function nil)
    (setq-local forward-sexp-function nil))
  (when (and (boundp 'transpose-sexps-function) (eq transpose-sexps-function #'treesit-transpose-sexps))
    (setq transpose-sexps-function nil)
    (setq-local transpose-sexps-function nil))
  (when (and (boundp 'forward-sentence-function) (eq forward-sentence-function #'treesit-forward-sentence))
    (setq forward-sentence-function nil)
    (setq-local forward-sentence-function nil)))

(add-hook 'prog-mode-hook #'julian/fix-treesit-sexp-commands)
 
(defun julian/copy-magit-relative-filename ()
  (interactive)
  (let ((name (magit-file-relative-name)))
    (kill-new name)))

(provide 'my-keybinds)
