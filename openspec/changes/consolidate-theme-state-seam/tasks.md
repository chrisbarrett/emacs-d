# Tasks: consolidate-theme-state-seam

## 1. gfm library indirection

- [ ] 1.1 Write failing tests: `gfm-pretty-background-function` default
      returns the `default` face background; callouts tint faces derive
      from it; `gfm-pretty-theme-change-hook` run triggers face refresh
      while enabled and not after disable
- [ ] 1.2 Add `gfm-pretty-theme-change-hook` and
      `gfm-pretty-background-function` to gfm-pretty; move the two
      callouts add-hook registrations onto the library hook; replace
      `boundp`-guarded `+theme-default-background` reads with the
      function
- [ ] 1.3 Assert no file under `lisp/` (excluding generated
      `+autoloads.el`) references `+theme-changed-hook` or
      `+theme-default-background`; add a regression test for this

## 2. Move the cache to the theme module

- [ ] 2.1 Write failing tests: `+theme-default-background` is defined
      by theme lib; refresh hook registered at depth ≤ -95; cache is a
      real colour after theme load
- [ ] 2.2 Move the defvar and save-hook from `modules/tty/init.el` to
      `modules/theme/lib.el`; keep `+tty-clear-bg-h` in tty; verify
      tty's "refresh precedes clear" ordering still holds

## 3. Bridge in theme module

- [ ] 3.1 Write failing test: after theme switch,
      `gfm-pretty-theme-change-hook` runs and
      `gfm-pretty-background-function` returns the cached background
- [ ] 3.2 Wire the bridge in `modules/theme/` under
      `with-eval-after-load 'gfm-pretty`

## 4. Close out

- [ ] 4.1 Full `make test`; manual smoke: theme switch in a gui and a
      tty frame updates callout tints and emits no
      `Unable to load color` messages
- [ ] 4.2 Byte-compile and checkdoc clean on touched files
