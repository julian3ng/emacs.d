(define-skeleton julian/transaction-thunk
  "Insert transaction thunk"
  nil
  "async (transaction) => {"
  _
  "}")

(define-skeleton julian/console-notify
  "Insert iterm notification log"
  nil
  "console.log('\\x1b]9;" _ "\\x07');")

(define-skeleton julian/console-log
  "Insert console.log"
  nil
  "console.log(" _ ");")

(define-skeleton julian/console-json
  "Insert console.log(JSON.stringify(..., null, 4))"
  nil
  "console.log(" _ ", null, 4);")



(defvar-keymap julian/console-abbrev-map
  :doc "console.* abbrevs"
  "l" #'julian/console-log
  "j" #'julian/console-json
  "n" #'julian/console-notify
  )

(defvar-keymap julian/abbrev-map
  :doc "Julian's abbrevs"
  "c" julian/console-abbrev-map)

(which-key-add-keymap-based-replacements julian/abbrev-map
  "c" `("console" . ,julian/console-abbrev-map))

(keymap-set global-map "C-c C-s" julian/abbrev-map)


(provide 'my-skeletons)
