markdown-mode

(s "```" p n q n "```")
(js "```javascript" p n q n "```")
(ts "```typescript" p n q n "```")
(sh "```sh" p n q n "```")

;; GitHub extensions
;;
;; See: https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax

(nb "> [!NOTE]" n "> " q)
(tip "> [!TIP]" n "> " q)
(imp "> [!IMPORTANT]" n "> " q)
(wa "> [!WARNING]" n "> " q)
(ca "> [!CAUTION]" n "> " q)
