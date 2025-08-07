((magit-branch-configure)
 (magit-fetch "--prune" "--tags")
 (magit-log:magit-log-mode "-n256" "--follow" "--graph" "--decorate"))
