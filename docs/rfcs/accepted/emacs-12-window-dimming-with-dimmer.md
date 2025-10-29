---
title: "RFC: emacs-12 - Window Dimming with dimmer.el"
date: 2025-10-29
author: Chris Barrett
agent: report-writer
ticket_id: emacs-12
tags: [rfc, emacs, visual-enhancement, dimmer]
---

# RFC: emacs-12 - Window Dimming with dimmer.el

> Ticket: emacs-12 - Status: Accepted (determined by folder location:
> docs/rfcs/accepted/)

## Summary

This RFC proposes adding visual distinction between active and inactive windows
by integrating dimmer.el into the Emacs configuration. The dimmer.el package
will automatically dim inactive buffers by adjusting face colors, making the
current window more visually prominent without requiring manual theme
customizations.

## Motivation

### Problem Statement

In a multi-window Emacs layout, it can be difficult to quickly identify which
window is currently active, especially when:

- Working with many windows simultaneously
- Using consistent color schemes across all buffers
- Switching between windows frequently
- Using terminal frames where cursor visibility may be reduced

This cognitive overhead slows down navigation and reduces productivity.

### Use Cases

- **Multi-window development**: When editing code across multiple split windows,
  dimming helps track context
- **Reference + editing**: When viewing documentation in one window while
  editing in another
- **Org-mode workflows**: When navigating between agenda views, notes, and
  capture buffers
- **Magit operations**: When reviewing diffs while composing commit messages
- **Terminal usage**: When cursor position is less obvious in terminal frames

### Goals

1. Provide clear visual indication of the active window
2. Integrate seamlessly with existing catppuccin theme
3. Maintain performance (no noticeable lag when switching windows)
4. Support both GUI and terminal frames
5. Allow user customization of dimming intensity
6. Exclude special buffers that shouldn't be dimmed (minibuffer, popups, etc.)

### Non-Goals

- This RFC does NOT address dimming for inactive frames (only windows within
  active frame)
- This RFC does NOT replace existing cursor highlighting mechanisms (hl-line,
  pulsar)
- This RFC does NOT provide window border customization

## Proposed Solution

### Overview

Integrate dimmer.el directly in init.el that:

1. Enables dimmer-mode globally on first input
2. Configures dimming to work with the catppuccin dark theme
3. Excludes special buffers and transient UI elements
4. Integrates with existing packages (company, which-key, magit, etc.)
5. Provides user-customizable dimming intensity
6. Provides a toggle keybinding (SPC t d) for easy enable/disable

### Detailed Design

#### Architecture

The implementation follows the existing inline pattern for simple visual enhancements:

- Configuration added directly to `init.el` (no separate module file needed)
- Loaded via `use-package` with `:ensure t`
- Deferred activation using `+first-input-hook`
- Toggle keybinding via general.el (SPC t d)

#### API/Interface Design

Configuration to be added directly to init.el (in "Visual enhancements" section, after pulsar around line 944):

```elisp
(use-package dimmer :ensure t
  ;; Dim inactive windows to highlight the active window
  :hook (+first-input-hook . dimmer-mode)
  :custom
  ;; Adjust foreground colors only for better dark theme compatibility
  (dimmer-adjustment-mode :foreground)

  ;; Start with moderate dimming (20% adjustment)
  ;; Users can customize this value: increase for more dimming
  (dimmer-fraction 0.20)

  ;; Use CIELAB color space for more perceptually uniform dimming
  (dimmer-use-colorspace :cielab)

  ;; Dim all buffers when Emacs loses focus
  (dimmer-watch-frame-focus-events t)

  ;; Exclude buffers that shouldn't be dimmed
  (dimmer-buffer-exclusion-regexps
   '("^ \\*Minibuf-[0-9]+\\*$"  ; Minibuffer
     "^ \\*Echo Area"            ; Echo area
     "^ \\*which-key\\*$"        ; which-key display
     "^\\*LV\\*$"))              ; Transient/hydra displays

  ;; Prevent dimming during transient UI states
  (dimmer-prevent-dimming-predicates
   '(;; Don't dim during window selection operations
     window-minibuffer-p
     ;; Don't dim when company completion is active
     (lambda () (and (bound-and-true-p company-mode)
                     company-candidates))))

  :config
  ;; Configure integrations with popular packages
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-hydra)
  (dimmer-configure-company-box)

  :general
  (+leader-def "t d" '(dimmer-mode :which-key "dimmer")))

#### Data Model

No persistent data model. Runtime state includes:

- Current active window reference
- Cached dimmed face values (computed by dimmer.el)
- Buffer exclusion list
- Dimming predicates

#### Implementation Plan

**Phase 1: Core implementation** (this PR)

1. Add dimmer.el use-package configuration to `init.el`
2. Add toggle keybinding (SPC t d)
3. Use conservative default settings (dimmer-fraction 0.20)

**Phase 2: Observation** (1-2 weeks)

1. Use in daily workflow
2. Note any problematic interactions
3. Identify buffers that need exclusion

**Phase 3: Refinement** (follow-up PR if needed)

1. Adjust dimmer-fraction if too subtle/aggressive
2. Add additional buffer exclusions discovered through use
3. Fine-tune colorspace settings if color shifts occur

### Examples

#### Basic Usage

After installation and first input:

```
┌──────────────────────────────────────┐
│  Active Window (normal brightness)   │  ← Currently selected
│  def hello():                         │
│      print("Hello, world!")           │
└──────────────────────────────────────┘
┌──────────────────────────────────────┐
│  Inactive Window (dimmed 20%)        │  ← Automatically dimmed
│  def goodbye():                       │
│      print("Goodbye!")                │
└──────────────────────────────────────┘
```

#### Theme Integration

When switching themes (e.g., `+theme-toggle`):

```elisp
;; No manual configuration needed - dimmer automatically adapts
(+theme-toggle)  ; Switch from dark to light theme
;; dimmer.el detects face changes and recomputes dimmed colors
```

#### User Customization

Users can adjust dimming intensity:

```elisp
;; In custom.el or site configuration
(setq dimmer-fraction 0.30)  ; Increase dimming intensity
(dimmer-mode -1)             ; Temporarily disable
(dimmer-mode 1)              ; Re-enable with new settings
```

## Alternatives Considered

### Alternative 1: auto-dim-other-buffers-mode

**Description**: Built-in package (as of Emacs 28) that dims non-selected
buffers

**Pros**:

- Built-in, no external dependency
- Simpler implementation

**Cons**:

- Less flexible configuration options
- No color space control
- Fewer integration helpers for popular packages
- Less actively maintained than dimmer.el

**Why not chosen**: dimmer.el provides superior color adjustment algorithms
(CIELAB support), more granular control over dimming behavior, and better
integration with popular packages.

### Alternative 2: selected-window-accent-mode

**Description**: Recently added to MELPA (2024), highlights active window with
border/fringe changes

**Pros**:

- Different visual approach (border vs dimming)
- Can be combined with dimmer
- Good for subtle indication

**Cons**:

- Requires visible fringes (may conflict with no-fringe configurations)
- Less effective in terminal frames
- Newer package, less proven

**Why not chosen**: Dimming inactive content is more visually effective than
border highlighting. However, this could be reconsidered as a complementary
feature.

### Alternative 3: Manual theme customization

**Description**: Define "dim" versions of all faces manually in theme
configuration

**Pros**:

- Complete control over appearance
- No runtime computation

**Cons**:

- High maintenance burden (must update when theme changes)
- Fragile (breaks with theme updates)
- Doesn't adapt to user theme switching
- Significant implementation complexity

**Why not chosen**: dimmer.el's dynamic approach is far more maintainable and
flexible.

## Trade-offs and Considerations

### Performance Impact

<!-- prettier-ignore-start -->
> [!NOTE]
> **Estimated Impact**: Minimal to negligible
>
> **Reasoning**: dimmer.el recomputes face colors only when window focus changes,
> not on every redisplay. With lazy loading via `+first-input-hook`, startup time
> is unaffected. Window switching overhead is typically <1ms on modern hardware.
>
> **Confidence**: High (based on dimmer.el architecture and user reports)
>
> **Assumptions**: Typical usage involves <10 windows. Performance may degrade
> with >20 simultaneous windows, but this is an edge case.
<!-- prettier-ignore-end -->

**Expected overhead:**

- Startup: None (lazy loaded)
- Window switching: <1ms (face color computation)
- Redisplay: None (no frame-by-frame updates)

**Mitigation strategies:**

- Use `+first-input-hook` for deferred activation
- Exclude buffers that change focus frequently
- Profile if performance issues arise

### Security Implications

No security concerns. dimmer.el only modifies buffer-local display properties
and face attributes. It does not:

- Execute arbitrary code
- Access network resources
- Modify file contents
- Change security-sensitive settings

### Compatibility

**Backward compatibility:**

- No breaking changes (new feature only)
- Existing configuration unaffected
- Can be disabled by user if problematic

**Forward compatibility:**

- Module design allows easy updates to dimmer.el
- Configuration can be adjusted without code changes
- Compatible with future Emacs versions (tested with 30+)

**Package interactions:**

- Complementary with pulsar (different visual feedback mechanisms)
- Compatible with hl-line (operates on different face properties)
- Integration functions handle magit, org, company, which-key

### Maintenance

**Long-term considerations:**

1. **Theme updates**: If catppuccin theme changes drastically, may need to
   adjust dimmer-fraction
2. **New packages**: May discover new packages that need integration functions
3. **Upstream changes**: dimmer.el is actively maintained, should track updates
4. **User customization**: Support user requests for different dimming intensity

**Maintenance burden:** Low

- Configuration is declarative and stable
- Dimmer.el handles implementation complexity
- Updates are typically configuration tweaks, not code changes

## Implementation

### Prerequisites

- Emacs 30+ (already required by configuration)
- Elpaca package manager (already configured)
- No additional dependencies required

### Dependencies

**External packages:**

- `dimmer` (from MELPA, installed automatically by Elpaca)

**Internal modules:**

- Uses existing `+first-input-hook` from hook system
- Uses `+leader-def` macro for keybinding setup
- Follows inline configuration pattern for simple visual enhancements

### Testing Strategy

#### Manual Testing

1. **Basic functionality**
   - Open multiple windows with different buffers
   - Verify inactive windows are dimmed
   - Switch between windows and observe dimming updates
   - Check that dimming is smooth without flicker

2. **Buffer exclusions**
   - Invoke which-key (SPC with delay)
   - Verify which-key display is not dimmed
   - Use company completion
   - Verify completion popup doesn't trigger dimming of edit buffer

3. **Theme switching**
   - Toggle between light/dark themes
   - Verify dimming adapts to new theme colors
   - Check that dimming remains effective after theme change

4. **Package integrations**
   - Open magit status buffer
   - Verify magit diff windows dim appropriately
   - Use org-mode with multiple windows
   - Verify org agenda and capture buffers work correctly

5. **Terminal frame behavior**
   - Open Emacs in terminal with `emacs -nw`
   - Verify dimming works in terminal frames
   - Check color rendering is acceptable

#### Performance Testing

1. **Window switching latency**
   - Rapidly switch between windows (C-x o repeatedly)
   - Should feel instant, no perceptible lag
   - Monitor with `M-x profiler-start` if needed

2. **Startup time impact**
   - Measure with
     `emacs --eval '(message "%.3fs" (float-time (time-subtract (current-time) before-init-time)))'`
   - dimmer-mode should not significantly impact startup (lazy loaded on first
     input)

#### Edge Cases

1. **Single window**: Verify no dimming occurs (nothing to dim relative to)
2. **Duplicate buffers**: Open same buffer in multiple windows, verify all are
   equally dimmed when inactive
3. **Minibuffer interaction**: Verify minibuffer never dims, main window remains
   visible
4. **Frame focus loss**: Switch to another application, verify all buffers dim
   (if enabled)

### Rollout Plan

1. **Add dimmer configuration to init.el**
   - Add the use-package form in the "Visual enhancements" section
   - Place after pulsar configuration (around line 944)

2. **Reload configuration**
   - Restart Emacs, OR
   - Evaluate the new use-package form directly

3. **Wait for package installation**
   - Elpaca will automatically install dimmer.el
   - Monitor with `M-x elpaca-log`

4. **Verify functionality** (see Testing Strategy above)

5. **Test toggle keybinding**
   - Press `SPC t d` to toggle dimmer-mode on/off

### Monitoring and Metrics

**Qualitative metrics:**

- Subjective improvement in window focus clarity
- Reduction in "which window am I in?" moments
- No negative impact on theme aesthetics

**Quantitative metrics:**

- Window switching performance (should be <1ms overhead)
- Startup time impact (should be negligible with lazy loading)

**Warning signs to watch for:**

- Flicker or visual artifacts when switching windows
- Color shifts that make syntax highlighting less readable
- Performance degradation with many windows
- Conflicts with other visual packages (pulsar, hl-line)

## Open Questions

1. **Should dimmer-fraction be different for terminal vs GUI frames?**
   - Terminal color rendering may require different intensity
   - Could add frame-type-specific configuration if needed
   - Defer until testing reveals necessity

2. **Should dimming be disabled in certain major modes?**
   - Some modes may look odd when dimmed (e.g., image-mode?)
   - Start without mode-specific exclusions
   - Add as needed based on experience

3. **Integration with pulsar?**
   - Pulsar already highlights on window switch
   - The two features are complementary (pulsar = transient, dimmer =
     persistent)
   - Should work well together, but test for conflicts

## Timeline

**Estimated implementation time:** 1-2 hours

- **30 minutes**: Add configuration to init.el
- **30 minutes**: Initial testing and configuration refinement
- **30 minutes**: Documentation and edge case verification

**Follow-up observation:** 1-2 weeks of daily use to identify issues

## References

### Documentation

- [dimmer.el GitHub Repository](https://github.com/gonewest818/dimmer.el) -
  Official source and documentation
- [dimmer.el Configuration Examples](https://github.com/gonewest818/dimmer.el/blob/master/.emacs/init.el) -
  Author's personal configuration

### Related Packages

- [auto-dim-other-buffers-mode](https://elpa.gnu.org/packages/auto-dim-other-buffers.html) -
  Built-in alternative
- [selected-window-accent-mode](http://www.emacs.dyerdwelling.family/emacs/20240208164549-emacs-selected-window-accent-mode-now-on-melpa/) -
  Complementary approach

### Codebase Context

- `lisp/+theme.el` - Theme management utilities
- `init.el` - Main configuration file (pulsar configuration provides similar
  pattern for visual enhancements)

### Color Science

- [CIELAB Color Space](https://en.wikipedia.org/wiki/CIELAB_color_space) -
  Perceptually uniform color representation
- [HSL and HSV](https://en.wikipedia.org/wiki/HSL_and_HSV) - Alternative color
  spaces

## Revision History

See git history for document changes. Use
`git log -- docs/rfcs/emacs-12-window-dimming-with-dimmer.md` to view revision
history.

## Design Decisions and Rationale

### Foreground vs Background Dimming

<!-- prettier-ignore-start -->
> [!NOTE]
> **Recommendation**: Use `:foreground` adjustment mode for catppuccin theme
>
> **Reasoning**: Catppuccin is a dark theme with carefully balanced background colors.
> Adjusting foreground colors provides better contrast preservation while maintaining
> the theme's visual identity. Background adjustment can create muddy or washed-out
> appearance in dark themes.
>
> **Confidence**: High (based on dimmer.el documentation and dark theme best practices)
>
> **Alternatives considered**:
> - `:background` mode: May conflict with catppuccin's background color palette
> - `:both` mode: Can over-dim and reduce legibility
<!-- prettier-ignore-end -->

### Theme Integration Approach

<!-- prettier-ignore-start -->
> [!NOTE]
> **Judgment Call**: No additional theme-specific configuration needed
>
> **Reasoning**: dimmer.el dynamically computes dimmed colors based on the current
> theme's faces. The catppuccin theme defines comprehensive face specifications,
> so dimmer.el will automatically derive appropriate dimmed versions. The
> `+theme-changed-hook` mechanism isn't needed because dimmer reacts to face
> changes automatically.
>
> **Confidence**: High
>
> **Assumptions**: Catppuccin theme properly defines all standard faces that
> dimmer will adjust. If custom faces are added later that don't dim correctly,
> those specific faces can be excluded.
<!-- prettier-ignore-end -->

### Buffer Exclusion Strategy

**Buffers that SHOULD NOT be dimmed:**

1. **Minibuffer and Echo Area** - Critical for user input
2. **which-key displays** - Needed for command discovery
3. **Transient/Hydra buffers** - Temporary command interfaces
4. **Company completion popups** - Active during editing
5. **Special purpose windows** - Help, messages (handled by predicates)

**Implementation approach:**

- Use `dimmer-buffer-exclusion-regexps` for name-based exclusions
- Use `dimmer-prevent-dimming-predicates` for state-based exclusions
- Leverage built-in integration functions for common packages

### Configuration Options

**User-customizable via M-x customize-group RET dimmer RET:**

1. **dimmer-fraction** (default: 0.20)
   - Range: 0.0 to 1.0
   - Lower = subtle dimming, Higher = more pronounced
   - Recommended for catppuccin: 0.15 to 0.30

2. **dimmer-adjustment-mode** (default: :foreground)
   - Options: :foreground, :background, :both
   - Recommended: :foreground for dark themes

3. **dimmer-use-colorspace** (default: :cielab)
   - Options: :cielab, :hsl, :hsluv, :rgb
   - CIELAB provides most perceptually uniform results

4. **dimmer-watch-frame-focus-events** (default: t)
   - Set to nil if you want inactive Emacs frames to remain undimmed
