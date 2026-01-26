# Project Feature Spec

Frame-isolated project management using built-in project.el and beframe.

## Files

| File                   | Purpose                                   |
| :--------------------- | :---------------------------------------- |
| `init/init-project.el` | project.el configuration, rescan command  |
| `config/mod-beframe.el`| Frame isolation, project-switch-beframed  |
| `lisp/+git.el`         | Repository display name utilities         |

## External Packages

| Package  | Purpose                              |
| :------- | :----------------------------------- |
| beframe  | Buffer-frame association management  |

## Behaviors

### Project Discovery

| Setting                 | Value                           | Effect                                      |
| :---------------------- | :------------------------------ | :------------------------------------------ |
| `project-vc-ignores`    | `(".cache/")`                   | Ignore .cache/ in project file searches     |
| `project-list-exclude`  | nix-store + hidden-dir filter   | Exclude /nix/store/ and hidden directories  |

**Hidden directory filter:** Excludes projects in any hidden directory (containing `/./`), except for `~/.config/` paths.

**Auto-remembered projects:** On startup, `user-emacs-directory` and `org-directory` are automatically added to the project list via `project-remember-project`.

### Project Rescan

| Binding     | Command            | Effect                              |
| :---------- | :----------------- | :---------------------------------- |
| `C-x p R`   | `+projects-rescan` | Re-scan all remembered project dirs |

The rescan command tracks directories passed to `project-remember-projects-under` via advice, storing them in `+project-scan-dirs-alist`. Calling `+projects-rescan` re-invokes `project-remember-projects-under` for each tracked directory.

### Project Switch Command

When switching projects (via `project-switch-project`), the default action is `+project-switch-magit-status`:

1. Opens `magit-status` if the project root contains a `.git` directory
2. Falls back to `dired` if not a git repository

### Frame Isolation (Beframe)

Beframe mode associates buffers with frames, enabling project-specific buffer lists.

**Global keybindings:**

| Binding | Command                  | Effect                               |
| :------ | :----------------------- | :----------------------------------- |
| `s-t`   | `project-switch-beframed`| Switch project with frame isolation  |
| `M-W`   | `project-switch-beframed`| Alternate binding for TTY            |
| `C-x p p`| `project-switch-beframed`| Via project-prefix-map               |

**`project-switch-beframed` behavior:**

1. **With prefix arg:** Switch project in current frame (no isolation)
2. **Without prefix arg:**
   - If a frame already exists for the project, switch to it
   - If the current frame is the sole frame with no project, reuse it
   - Otherwise, create a new frame for the project

**Frame parameters set:**
- `project-root`: The project's root directory
- `name`: Display name from `+git-repo-display-name` or directory basename

**`+git-repo-display-name`:** For GitHub repositories, returns `owner/repo` format by parsing the remote URL. Returns `nil` for non-GitHub repos.

### Consult Buffer Restriction

When `beframe-mode` is active, `consult--buffer-query` is advised to filter results to only buffers in the current beframe context.

### Strict Project Isolation

| Variable                             | Default | Effect                                      |
| :----------------------------------- | :------ | :------------------------------------------ |
| `+beframe-strict-project-isolation-p`| `nil`   | Switch frames when reading project files    |

When enabled, opening a file from a different project (via `project--read-file-name`) automatically switches to that project's frame.

### Eat Terminal Integration

| Function        | Effect                                          |
| :-------------- | :---------------------------------------------- |
| `eat-beframed`  | Run eat with buffer name scoped to frame name   |

Creates eat buffers named `*eat FRAME-NAME*` for frame-specific terminal sessions. With prefix arg, creates a new session.

## API

### Commands

| Command                  | Interactive | Description                           |
| :----------------------- | :---------- | :------------------------------------ |
| `+projects-rescan`       | Yes         | Re-scan all tracked project dirs      |
| `project-switch-beframed`| Yes         | Switch project with frame isolation   |
| `eat-beframed`           | Yes         | Run eat scoped to current frame       |

### Functions

| Function                 | Arguments | Description                           |
| :----------------------- | :-------- | :------------------------------------ |
| `+git-repo-display-name` | None      | Get owner/repo name for GitHub repos  |

### Variables

| Variable                              | Type    | Description                              |
| :------------------------------------ | :------ | :--------------------------------------- |
| `+project-scan-dirs-alist`            | alist   | Directories to rescan (auto-populated)   |
| `+beframe-strict-project-isolation-p` | boolean | Enable strict frame isolation on read    |

## Testable Properties

1. `project-vc-ignores` contains `".cache/"`
2. `project-list-exclude` filters `/nix/store/` paths
3. Hidden directories (except `~/.config/`) are excluded from project list
4. `project-switch-commands` is `+project-switch-magit-status`
5. `s-t` and `M-W` are bound to `project-switch-beframed`
6. `beframe-mode` is enabled by default
7. `+git-repo-display-name` returns `owner/repo` for GitHub URLs
8. `+projects-rescan` is bound to `R` in `project-prefix-map`
9. `consult--buffer-query` advice filters by beframe context when beframe-mode active
