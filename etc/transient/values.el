((magit-branch-configure)
 (magit-diff:magit-diff-mode "--ignore-space-change" "--no-ext-diff" "--stat")
 (magit-fetch "--prune" "--tags")
 (magit-log:magit-log-mode "-n256" "--follow" "--graph" "--decorate"))
