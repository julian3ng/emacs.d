(require 'transient)

(setq skeleton-end-newline nil)
(define-skeleton julian/transaction-thunk
  "Insert transaction thunk"
  nil
  "async (transaction) => {"
  _
  "}")

(define-skeleton julian/console-something
  "Insert console.something"
  nil
  "console." str "(" _ ");")

(define-skeleton julian/notify
  "Insert iterm notification log"
  nil
  "'\\x1b]9;', " _ ", '\\x07'")

(define-skeleton julian/json
  "Insert JSON.stringify"
  nil
  "JSON.stringify(" _ ", null, 4)")


(define-skeleton julian/color
  "Insert console color code"
  nil
  "'\\x1b[" str "m', " _ ", '\\x1b[m'")

(define-skeleton julian/mark
  "Insert console mark for iterm"
  nil
  "'\\x1b]1337;SetMark\\x07', " _)

(define-skeleton julian/js-block-comment
  "Insert /** */"
  nil
  "/**
" _ "
*/")


(defun julian/make-console-log (r1 r2)
  (interactive "r")
  (let* ((args (transient-args transient-current-command))
         (red (transient-arg-value "--red" args))
         (green (transient-arg-value "--green" args))
         (blue (transient-arg-value "--blue" args))
         (bright (transient-arg-value "--bright" args))
         (json (transient-arg-value "--json" args))
         (level (or (transient-arg-value "--level=" args) "log"))
         (notification (transient-arg-value "--notification" args))
         (mark (transient-arg-value "--mark" args))
         (color (when (or red green blue) (+ (if bright 90 30) (* 4 (if blue 1 0)) (* 2 (if green 1 0)) (if red 1 0))))
         (do-kill mark-active))
    ;; TODO: Better way of managing region?
    (when do-kill
      (kill-region r1 r2))
    (let ((skeletons nil)
          (mark-active nil))
      (when json (push #'julian/json skeletons))
      (when notification (push #'julian/notify skeletons))
      (when (not (null color)) (push #'(lambda () (julian/color (number-to-string color))) skeletons))
      (when mark (push #'julian/mark skeletons))
      (message "%s" level)
      (push #'(lambda () (julian/console-something level)) skeletons)
      (dolist (s skeletons)
        (funcall s)))
    (when do-kill
      (yank))))

(transient-define-prefix julian/comment ()
  "Comment builder"
  [("d" "documentation" julian/js-block-comment :transient nil)])

(transient-define-prefix julian/console ()
  "Console statement builder"
  :incompatible '(("--notification" "--red")
                  ("--notification" "--green")
                  ("--notification" "--blue")
                  ("--notification" "--bright"))
  ["Options"
   ("!" "bright" ("-!" "--bright"))
   ("r" "red" ("-r" "--red"))
   ("g" "green" ("-g" "--green"))
   ("b" "blue" ("-b" "--blue"))
   ("j" "JSON" "--json")
   ("n" "Notification" "--notification")
   ("m" "Mark" "--mark")
   ("-l" "Log level" "--level=" :choices ("error" "warn" "log" "info" "debug"))]
  [("RET" "confirm" julian/make-console-log :transient nil)])

(transient-define-prefix julian/main-menu ()
  "Julian's main menu"
  [("c" "console.log" julian/console :transient nil)
   (";" "comments" julian/comment :transient nil)
   ])

(global-set-key (kbd "s-j") #'julian/main-menu)

(provide 'my-skeletons)
