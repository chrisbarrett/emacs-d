# Feature: Theme

Theme management with automatic light/dark switching based on system preferences.

## Summary

The theme feature provides:

- Automatic detection of system light/dark mode (macOS, Linux)
- Theme switching with a single command
- Hook for theme change notifications
- Custom face overrides for consistency across themes
- Frame parameter synchronization (borders, dividers)
- Support for both built-in (modus) and external (catppuccin) themes

## Dependencies

### External Packages

| Package         | Purpose                          |
| :-------------- | :------------------------------- |
| catppuccin-theme | Primary dark theme              |

### Built-in Features

| Feature      | Purpose                          |
| :----------- | :------------------------------- |
| modus-themes | Light theme (modus-operandi-tinted) |
| color        | Color manipulation utilities     |
| custom       | Face customization               |

## Behavior

### B1: Early Theme Configuration

**Given** Emacs starts
**When** early-init.el loads
**Then**:
- `+theme-light` is set to `modus-operandi-tinted`
- `+theme-dark` is set to `modus-vivendi` (fallback)
- Font faces are configured (Fira Code, Fira Sans)
- `+theme-update` is queued for `after-init-hook`
- Frame parameters are synchronized

### B2: Theme Loading

**Given** `after-init-hook` runs
**When** `+theme-update` is called
**Then**:
- System theme preference is queried
- All currently enabled themes are disabled
- The appropriate theme is loaded
- `+theme-changed-hook` is run

### B3: System Theme Detection

**Given** the system is macOS
**When** `+system-theme-query` is called
**Then** it executes `defaults read -g AppleInterfaceStyle`

**Given** the system is GNU/Linux
**When** `+system-theme-query` is called
**Then** it executes `gsettings get org.gnome.desktop.interface gtk-theme`

**Given** the query result contains "dark"
**When** `+theme-for-system-theme` is called
**Then** it returns `+theme-dark`

**Given** the query result does not contain "dark"
**When** `+theme-for-system-theme` is called
**Then** it returns `+theme-light`

### B4: Theme Override

**Given** `+theme-override` is set to a theme symbol
**When** `+theme-for-system-theme` is called
**Then** it returns `+theme-override` (ignoring system preference)

### B5: Manual Theme Switching

**Given** the user calls `+theme-dark`
**Then**:
- All enabled themes are disabled
- `+theme-dark` theme is loaded
- `+theme-changed-hook` is run

**Given** the user calls `+theme-light`
**Then**:
- All enabled themes are disabled
- `+theme-light` theme is loaded
- `+theme-changed-hook` is run

### B6: Catppuccin Dark Theme

**Given** init-theme.el loads
**When** catppuccin-theme is installed
**Then**:
- `+theme-dark` is overridden to `catppuccin`
- `+theme-update` is called to apply the theme

### B7: Custom Face Overrides

**Given** the user customization loads
**Then** these faces are defined:
- `region` inherits `lazy-highlight` (light backgrounds)
- `iedit-occurrence` inherits `query-replace`
- `font-lock-delimiter-face` inherits `shadow`

### B8: Frame Parameter Sync

**Given** `+theme-changed-hook` runs
**When** `+sync-frame-parameters` is called
**Then**:
- `right-divider-width` is set to 10
- `internal-border-width` is set to 10
- macOS frames are undecorated
- Fringe and divider colors match background

### B9: Dark Theme Detection

**Given** a theme is loaded
**When** `+theme-dark-p` is called
**Then** it returns non-nil if background luminance < 50%

## Provided API

### Variables

| Variable             | Purpose                                    |
| :------------------- | :----------------------------------------- |
| `+theme-light`       | Symbol of light theme to use               |
| `+theme-dark`        | Symbol of dark theme to use                |
| `+theme-override`    | If set, overrides system detection         |
| `+theme-changed-hook`| Hook run after theme changes               |

### Functions

| Function              | Purpose                                   |
| :-------------------- | :---------------------------------------- |
| `+theme-update`       | Sync theme with system preference         |
| `+theme-dark`         | Switch to dark theme manually             |
| `+theme-light`        | Switch to light theme manually            |
| `+theme-dark-p`       | Return non-nil if current theme is dark   |
| `+theme-for-system-theme` | Return theme for current system mode  |
| `+system-theme-query` | Query system for dark mode preference     |
| `+sync-frame-parameters` | Update frame borders/dividers          |

### Keybindings

| Key       | Command       | Condition |
| :-------- | :------------ | :-------- |
| `SPC t D` | `+theme-dark` | Leader    |
| `SPC t L` | `+theme-light`| Leader    |

## Properties to Verify

### P1: Theme Variables Set

```elisp
;; Given early-init has loaded
;; Then theme variables are configured
(should +theme-light)
(should +theme-dark)
```

### P2: System Query Dispatches

```elisp
;; Given system-type is darwin
;; When +system-theme-query is called
;; Then it returns a string (not error)
(should (stringp (+system-theme-query 'darwin)))
```

### P3: Theme Detection Logic

```elisp
;; Given +theme-dark is 'catppuccin
;; And +theme-light is 'modus-operandi-tinted
;; When system reports dark mode
;; Then +theme-for-system-theme returns +theme-dark
(let ((+theme-dark 'test-dark)
      (+theme-light 'test-light)
      (+theme-override nil))
  (cl-letf (((symbol-function '+system-theme-query)
             (lambda (_) "dark")))
    (should (eq 'test-dark (+theme-for-system-theme)))))
```

### P4: Theme Override Takes Precedence

```elisp
;; Given +theme-override is set
;; When +theme-for-system-theme is called
;; Then it returns +theme-override
(let ((+theme-override 'custom-theme))
  (should (eq 'custom-theme (+theme-for-system-theme))))
```

### P5: Theme Changed Hook Runs

```elisp
;; Given +theme-changed-hook has a function
;; When +theme-update is called
;; Then the hook function runs
(let ((ran nil))
  (add-hook '+theme-changed-hook (lambda () (setq ran t)) nil t)
  (+theme-update)
  (should ran))
```

### P6: Dark Theme Detection

```elisp
;; Given a dark theme is loaded
;; When +theme-dark-p is called
;; Then it returns non-nil
(load-theme 'modus-vivendi t)
(should (+theme-dark-p))
```

### P7: Light Theme Detection

```elisp
;; Given a light theme is loaded
;; When +theme-dark-p is called
;; Then it returns nil
(load-theme 'modus-operandi t)
(should-not (+theme-dark-p))
```

### P8: Frame Parameters Applied

```elisp
;; Given +sync-frame-parameters is called
;; Then frame has correct border widths
(should (= 10 (frame-parameter nil 'internal-border-width)))
(should (= 10 (frame-parameter nil 'right-divider-width)))
```

### P9: Custom Faces Defined

```elisp
;; Given init-theme has loaded
;; Then custom faces inherit correctly
(should (eq 'shadow
            (face-attribute 'font-lock-delimiter-face :inherit nil t)))
```

## Files

| File             | Purpose                              |
| :--------------- | :----------------------------------- |
| lisp/+theme.el   | Theme detection and switching        |
| init/init-theme.el | Theme package configuration        |
| early-init.el    | Initial theme setup (partial)        |
