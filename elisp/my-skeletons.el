(setq skeleton-end-newline nil)
(define-skeleton julian/transaction-thunk
  "Insert transaction thunk"
  nil
  "async (transaction) => {"
  _
  "}")

(define-skeleton julian/console-log
  "Insert console.log"
  nil
  "console.log('" _ "');")

(define-skeleton julian/notify
  "Insert iterm notification log"
  nil
  "\\x1b]9;" _ "\\x07")

(define-skeleton julian/json
  "Insert console.log(JSON.stringify(..., null, 4))"
  nil
  "JSON.stringify(" _ ", null, 4)")

(define-skeleton julian/red
  "Insert \x1b[91m...\x1b[m"
  nil
  "\\x1b[91m" _ "\\x1b[m")

;; (defvar-keymap julian/console-abbrev-map
;;   :doc "console.* abbrevs"
;;   "l" #'julian/console-log
;;   "j" #'julian/console-json
;;   "n" #'julian/console-notify
;;   )

;; (defvar-keymap julian/abbrev-map
;;   :doc "Julian's abbrevs"
;;   "c" julian/console-abbrev-map)

;; (which-key-add-keymap-based-replacements julian/abbrev-map
;;   "c" `("console" . ,julian/console-abbrev-map))

;; (keymap-set global-map "s-s" julian/abbrev-map)

(transient-define-prefix julian/console-menu ()
  "Julian's console log builders"
  [("n" "notify" julian/notify :transient t)
   ("j" "json" julian/json :transient t)
   ("1" "red" julian/red :transient t)])

(defun julian/insert-console-then ()
  (interactive)
  (julian/console-log)
  (julian/console-menu))

(transient-define-prefix julian/main-menu ()
  "Julian's main menu"
  [("c" "console.log" julian/insert-console-then :transient t)])

(global-set-key (kbd "s-j") #'julian/main-menu)

(provide 'my-skeletons)

