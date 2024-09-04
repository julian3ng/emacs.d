(define-skeleton julian/transaction-thunk
  "Insert transaction thunk"
  nil
  "async (transaction) => {"
  _
  "}")

(when (boundp 'typescript-ts-base-mode-abbrev-table)
  (define-abbrev typescript-ts-base-mode-abbrev-table "atx"
    "" 'julian/transaction-thunk))

(provide 'my-skeletons)
