markdown-mode

(s "```" p n q n "```")
(js "```javascript" p n q n "```")
(ts "```typescript" p n q n "```")
(sh "```sh" p n q n "```")

;; GitHub extensions
;;
;; See: https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax

(nb "<!-- prettier-ignore-start -->" n
    "> [!NOTE]" n
    "> " q n
    "<!-- prettier-ignore-end -->")

(tip "<!-- prettier-ignore-start -->" n
     "> [!TIP]" n
     "> " q n
     "<!-- prettier-ignore-end -->")

(imp "<!-- prettier-ignore-start -->" n
     "> [!IMPORTANT]" n
     "> " q n
     "<!-- prettier-ignore-end -->")

(wa "<!-- prettier-ignore-start -->" n
    "> [!WARNING]" n
    "> " q n
    "<!-- prettier-ignore-end -->")

(ca "<!-- prettier-ignore-start -->" n
    "> [!CAUTION]" n
    "> " q n
    "<!-- prettier-ignore-end -->")
