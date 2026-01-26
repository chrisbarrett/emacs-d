# Dired

Directory browser configuration with icons, enhanced fonts, and wdired editing.

## Files

| File               | Purpose                                 |
| :----------------- | :-------------------------------------- |
| `init/init-dired.el` | Dired, dired-x, wdired, nerd-icons, diredfl |

## Packages

| Package          | Source   | Purpose                           |
| :--------------- | :------- | :-------------------------------- |
| dired            | built-in | Core directory browser            |
| dired-x          | built-in | Extra dired functionality         |
| wdired           | built-in | Writable dired buffers            |
| nerd-icons       | elpaca   | Icon font library                 |
| nerd-icons-dired | elpaca   | Icons in dired listings           |
| diredfl          | elpaca   | Enhanced font-lock for dired      |

## Behaviors

### Visual Display

| When                        | Then                                           |
| :-------------------------- | :--------------------------------------------- |
| Dired buffer opens          | `dired-hide-details-mode` enabled              |
| Dired buffer opens          | `hl-line-mode` enabled                         |
| Dired buffer opens          | Nerd font icons shown for files                |
| Dired buffer opens (local)  | Uses long listing switches with grouping       |
| Dired buffer opens (remote) | Uses simple `-al` listing                      |
| Dired buffer opens          | `dired-omit-mode` enabled (hides dotfiles)     |

### File Operations

| Behavior                         | Setting                            |
| :------------------------------- | :--------------------------------- |
| Recursive copies                 | Always (no confirmation)           |
| Recursive deletes                | Always (no confirmation)           |
| Delete action                    | Moves to system trash              |
| DWIM target                      | Enabled (uses other dired window)  |
| Auto-revert                      | When directory changed             |
| Create destination dirs          | Ask before creating                |
| VC-aware rename                  | Enabled (uses version control)     |
| Open new dired buffer            | Kills previous dired buffer        |

### Omit Patterns

Files matching these patterns are hidden by default:

- Files starting with `.` (dotfiles)
- `__pycache__` directories
- `node_modules` directories

Garbage file patterns (for `dired-do-flagged-delete`):

- `.log`, `.toc`, `.dvi`, `.bak`, `.orig`, `.rej`, `.aux`, `.DS_Store`

### Wdired Editing

| When                              | Then                           |
| :-------------------------------- | :----------------------------- |
| `C-c C-e` in dired buffer         | Enters wdired mode             |
| Save wdired buffer                | Renames files to match changes |

## API

### Keybindings (Local Leader)

| Key   | Command                | Description            |
| :---- | :--------------------- | :--------------------- |
| `, t` | dired-toggle-marks     | Toggle all marks       |
| `, d` | dired-hide-details-mode| Toggle details view    |
| `, h` | dired-omit-mode        | Toggle hidden files    |
| `, l` | (prefix)               | Filesystem links       |
| `, l s` | dired-do-symlink     | Create absolute symlink|
| `, l r` | dired-do-relsymlink  | Create relative symlink|
| `, l h` | dired-do-hardlink    | Create hard link       |
| `, s` | (prefix)               | Subdirectory           |
| `, s i` | dired-insert-subdir  | Insert subdir inline   |
| `, s x` | dired-kill-subdir    | Kill subdir listing    |

### Keybindings (Global)

| Key       | Command                   | Description         |
| :-------- | :------------------------ | :------------------ |
| `C-c C-e` | wdired-change-to-wdired-mode | Edit filenames   |

## Listing Switches

### Local Directories

```
--almost-all --human-readable --group-directories-first --no-group
```

- `--almost-all`: Show hidden files except `.` and `..`
- `--human-readable`: Human-readable file sizes
- `--group-directories-first`: Directories listed before files
- `--no-group`: Omit group column

### Remote Directories (TRAMP)

```
-al
```

Simplified flags for compatibility with remote systems.

## Testable Properties

1. Opening dired buffer enables `dired-hide-details-mode`
2. Opening dired buffer enables `hl-line-mode`
3. Opening dired buffer enables `dired-omit-mode`
4. Nerd icons appear in dired listings
5. Local directories use long listing switches
6. Remote directories use simple `-al` switches
7. `C-c C-e` enters wdired mode
8. `delete-by-moving-to-trash` is t
9. `dired-dwim-target` is t
