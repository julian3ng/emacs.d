(define-skeleton julian/transaction-thunk
  "Insert transaction thunk"
  nil
  "async (transaction) => {"
  _
  "}")

(define-skeleton julian/console-log-notify
  "Insert iterm notification log"
  nil
  "console.log('\\x1b]9;"
  _
  "\\x07');"
  )

(when (boundp 'typescript-ts-base-mode-abbrev-table)
  (define-abbrev typescript-ts-base-mode-abbrev-table "atx"
    "" 'julian/transaction-thunk)

  (define-abbrev typescript-ts-base-mode-abbrev-table "clnotify"
    "" 'julian/console-log-notify))

(provide 'my-skeletons)
