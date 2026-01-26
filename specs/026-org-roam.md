# 026-org-roam

Zettelkasten implementation using org-roam for networked notes with backlinks.

## Files

| File              | Purpose                                   |
| :---------------- | :---------------------------------------- |
| init-org-roam.el  | org-roam setup, capture, node display     |
| init-nursery.el   | org-roam extensions from nursery package  |

## Packages

| Package                      | Purpose                            |
| :--------------------------- | :--------------------------------- |
| org-roam                     | Core zettelkasten framework        |
| nursery (org-roam-review)    | Spaced repetition review workflow  |
| nursery (org-roam-search)    | Full-text search across nodes      |
| nursery (org-roam-links)     | Link visualization                 |
| nursery (org-roam-rewrite)   | Node refactoring operations        |
| nursery (org-roam-dblocks)   | Dynamic blocks in org-roam         |
| nursery (org-roam-slipbox)   | Slipbox/category organization      |
| nursery (timekeep)           | Time tracking integration          |

## Behaviors

### Database Sync

| Condition                  | Behavior                                      |
| :------------------------- | :-------------------------------------------- |
| After org-roam loads       | Enable `org-roam-db-autosync-mode`            |
| File listing               | Prefer fd/fdfind/rg over find for performance |

### Node Discovery

| Variable                | Value                                               |
| :---------------------- | :-------------------------------------------------- |
| `org-roam-extract-new-file-path` | `notes/${slug}.org`                        |
| `org-roam-mode-sections`| Backlinks (unique), Reflinks                        |

### Capture

| Template | Key | Target                    | Options                       |
| :------- | :-- | :------------------------ | :---------------------------- |
| default  | `d` | `notes/${slug}.org`       | immediate-finish, unnarrowed  |

### Sensitive Node Filtering

| Tag                        | Effect                                  |
| :------------------------- | :-------------------------------------- |
| `daily`                    | Hidden from default node-find           |
| `sensitive`                | Hidden from default node-find           |
| `private`                  | Hidden from default node-find           |
| With prefix arg            | Show all nodes including sensitive      |

### Node Display

| Aspect                     | Behavior                                |
| :------------------------- | :-------------------------------------- |
| Completion template        | Function-based OLP display (fast)       |
| OLP format                 | `file: path: title` with faces          |
| Marginalia annotation      | `@slipbox #tag1 #tag2` format           |

### ID Creation

| Condition                  | Behavior                                |
| :------------------------- | :-------------------------------------- |
| Storing links in roam file | Create ID if interactive                |
| `org-id-link-to-org-use-id`| Set to `create-if-interactive` locally  |

### Font Lock

| Pattern         | Face                 | Mode     |
| :-------------- | :------------------- | :------- |
| `^LINKS:`       | `org-special-keyword`| org-mode |

## Keybindings

### Org Mode

| Key            | Command                | Context        |
| :------------- | :--------------------- | :------------- |
| `<local> TAB`  | `org-roam-buffer-toggle` | org-mode     |
| `C-c C-i`      | `org-roam-node-insert`   | org-mode     |
| `<local> l a`  | `org-roam-alias-add`     | org-mode     |
| `<local> l x`  | `org-roam-alias-remove`  | org-mode     |
| `<local> r r`  | `org-roam-rewrite-rename`| org-mode     |
| `<local> r i`  | `org-roam-rewrite-inline`| org-mode     |
| `<local> r e`  | `org-roam-rewrite-extract`| org-mode    |
| `<local> r D`  | `org-roam-rewrite-remove`| org-mode     |
| `<local> r R`  | `org-roam-slipbox-refile`| org-mode     |

### Roam Buffer (Backlinks)

| Key       | Command                         | States         |
| :-------- | :------------------------------ | :------------- |
| `M-p`     | `magit-section-backward-sibling`| All            |
| `M-n`     | `magit-section-forward-sibling` | All            |
| `TAB`     | `magit-section-toggle`          | All            |
| `C-TAB`   | `magit-section-cycle`           | All            |
| `S-TAB`   | `magit-section-cycle-global`    | All            |
| `]`       | `magit-section-forward-sibling` | normal, visual |
| `[`       | `magit-section-backward-sibling`| normal, visual |
| `gj`      | `magit-section-forward-sibling` | normal, visual |
| `gk`      | `magit-section-backward-sibling`| normal, visual |
| `gr`      | `revert-buffer`                 | normal, visual |
| `gR`      | `revert-buffer`                 | normal, visual |
| `z1-z4`   | `magit-section-show-level-N`    | normal, visual |
| `za`      | `magit-section-toggle`          | normal, visual |
| `zc`      | `magit-section-hide`            | normal, visual |
| `zC`      | `magit-section-hide-children`   | normal, visual |
| `zo`      | `magit-section-show`            | normal, visual |
| `zO`      | `magit-section-show-children`   | normal, visual |
| `zm`      | `magit-section-show-level-2-all`| normal, visual |
| `zr`      | `magit-section-show-level-4-all`| normal, visual |
| `C-j`     | `magit-section-forward`         | normal, visual |
| `C-k`     | `magit-section-backward`        | normal, visual |

### Review Mode

| Key       | Command                         | States |
| :-------- | :------------------------------ | :----- |
| `/`       | `org-roam-review-modify-tags`   | normal |
| `g r`     | `org-roam-review-refresh`       | normal |

### Global

| Key       | Command                         | Condition           |
| :-------- | :------------------------------ | :------------------ |
| `<f12>`   | `timekeep-start`                | Not clocking        |
| `<f12>`   | `timekeep-stop`                 | Currently clocking  |

## Nursery Extensions

### org-roam-review

| Feature                    | Behavior                               |
| :------------------------- | :------------------------------------- |
| Spaced repetition          | Review nodes on schedule               |
| Mode hook                  | `toggle-truncate-lines` enabled        |

### org-roam-refill-previews

| Feature                    | Behavior                               |
| :------------------------- | :------------------------------------- |
| Preview postprocessing     | Refill paragraph previews              |

### org-roam-lazy-previews

| Feature                    | Behavior                               |
| :------------------------- | :------------------------------------- |
| Performance                | Load previews lazily                   |

### org-roam-dblocks

| Feature                    | Behavior                               |
| :------------------------- | :------------------------------------- |
| Auto-update mode           | Enabled via `org-mode-hook`            |

### org-roam-slipbox

| Feature                    | Behavior                               |
| :------------------------- | :------------------------------------- |
| Tag mode                   | Enabled after org-roam loads           |
| Slipbox annotation         | Shows `@slipbox` in marginalia         |

## API

### Commands

| Command                        | Purpose                              |
| :----------------------------- | :----------------------------------- |
| `+org-roam-node-find`          | Find node, filtering sensitive       |
| `org-roam-buffer-toggle`       | Toggle backlinks buffer              |
| `org-roam-node-insert`         | Insert link to node                  |
| `org-roam-alias-add`           | Add alias to current node            |
| `org-roam-alias-remove`        | Remove alias from current node       |
| `org-roam-review`              | Start review session                 |
| `org-roam-review-list-recently-added` | List recently created nodes   |
| `org-roam-search`              | Full-text search across nodes        |
| `org-roam-links`               | Visualize node links                 |
| `org-roam-rewrite-rename`      | Rename node                          |
| `org-roam-rewrite-inline`      | Inline node content                  |
| `org-roam-rewrite-extract`     | Extract region to new node           |
| `org-roam-rewrite-remove`      | Remove node                          |
| `org-roam-slipbox-refile`      | Refile node to different slipbox     |
| `timekeep-start`               | Start time tracking                  |
| `timekeep-stop`                | Stop time tracking                   |

### Functions

| Function                       | Purpose                              |
| :----------------------------- | :----------------------------------- |
| `+org-roam-node-sensitive-p`   | Predicate for non-sensitive nodes    |
| `+org-roam-node-formatted-olp` | Format node with OLP for completion  |
| `+org-roam-node-tags-annotator`| Marginalia annotator for node tags   |

### Variables

| Variable                       | Purpose                              |
| :----------------------------- | :----------------------------------- |
| `+org-roam-sensitive-tags`     | Tags marking sensitive nodes         |

## Testable Properties

1. `org-roam-db-autosync-mode` is enabled after org-roam loads
2. `+org-roam-node-find` filters nodes with tags in `+org-roam-sensitive-tags`
3. `+org-roam-node-find` with prefix arg shows all nodes
4. `org-roam-mode-map` has vim-style section navigation keys bound
5. `org-id-link-to-org-use-id` is `create-if-interactive` in roam files
6. `org-roam-review-mode-hook` enables `toggle-truncate-lines`
7. `org-roam-dblocks-autoupdate-mode` is enabled via `org-mode-hook`
8. `org-roam-slipbox-tag-mode` is enabled after org-roam loads
9. `<f12>` dispatches to start/stop based on clocking state
