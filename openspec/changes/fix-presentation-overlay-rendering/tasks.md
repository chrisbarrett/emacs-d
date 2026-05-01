## 1. Faces & shared primitives

- [ ] 1.1 Define `+presentation-focus-face` (theme-aware, muted bg,
      no `:extend`) with light/dark backgrounds; failing test asserts
      face exists and that `:extend` is unset
- [ ] 1.2 Define `+presentation-annotation-note-face`,
      `+presentation-annotation-tip-face`,
      `+presentation-annotation-warning-face`; each inherits from
      the matching `+markdown-gfm-callout-*-face` when available
      with a sensible fallback; failing test asserts inheritance
- [ ] 1.3 Add helper `+presentation--blend-toward-fg` mirroring
      `gfm-callouts--quote-face`'s blend trick (or copy the 5-line
      body); failing test asserts hex output for known input
- [ ] 1.4 Add helper `+presentation--make-border` (line-of-`─`
      with corner chars and a face) mirroring
      `gfm-callouts--make-border`; failing tests for `┌─...─┐`
      and `└─...─┘` shapes

## 2. Annotation validation

- [ ] 2.1 Failing tests for `+presentation--validate-annotation`:
      `:kind` accepted values; bad kind rejected with named value;
      missing kind accepted (defaults to inline at render)
- [ ] 2.2 Failing tests: `:severity` accepted values; bad severity
      rejected with named value; missing severity accepted
- [ ] 2.3 Failing tests for position-per-kind: inline accepts
      `"before"` / `"after"` / absent; callout rejects any
      `:position`; margin accepts `"left"` / `"right"` / `"before"`
      / `"after"` / absent
- [ ] 2.4 Failing test: unknown plist field on annotation rejected
- [ ] 2.5 Implement validation extension; tests green

## 3. Inline annotation rendering (correct position semantics)

- [ ] 3.1 Failing test: inline annotation with `position: "after"`
      produces an overlay whose start equals point-at-eol of the
      target line and whose `after-string` equals the annotation
      text
- [ ] 3.2 Failing test: inline annotation with `position: "before"`
      produces an overlay at point-at-bol with a `before-string`
- [ ] 3.3 Failing test: inline annotation severity face is applied
      to the rendered string (`get-text-property 0 'face`)
- [ ] 3.4 Implement `+presentation--render-inline-annotation`; tests
      green
- [ ] 3.5 Replace existing `+presentation--apply-annotations` body
      to dispatch on `:kind` (default inline); ensure existing
      annotation tests for narrative/file/diff still pass

## 4. Callout annotation rendering

- [ ] 4.1 Failing test for `+presentation--render-callout-annotation`:
      overlay anchored at point-at-eol of target line; `after-string`
      begins with `┌` and ends with `┘`; severity face on border
      characters; box width = max(80, content+2)
- [ ] 4.2 Failing test: callout label header includes the severity
      name (`NOTE` / `TIP` / `WARNING`)
- [ ] 4.3 Failing test: multi-line body wraps inside the box, with
      `align-to` padding to keep the right edge aligned
- [ ] 4.4 Implement `+presentation--render-callout-annotation`;
      tests green

## 5. Margin annotation rendering

- [ ] 5.1 Failing test: margin annotation with `position: "right"`
      produces an overlay carrying a `display` property of
      `(margin right-margin <text>)`
- [ ] 5.2 Failing test: margin annotation with `position: "left"`
      maps to `(margin left-margin <text>)`
- [ ] 5.3 Failing test: when any margin annotation is present, the
      buffer's `right-margin-width` is set to at least 12 (or
      `left-margin-width` for left-side); prior value is captured
      as a restorer in `:render-state`
- [ ] 5.4 Failing test: cleanup restores margin width to its prior
      value
- [ ] 5.5 Failing test: position values `"before"` / `"after"` on a
      margin annotation are normalised to `"right"`
- [ ] 5.6 Implement `+presentation--render-margin-annotation` and
      margin-width setup/restore; tests green

## 6. Focus highlight rework

- [ ] 6.1 Failing test: `+presentation--render-file` with `:focus`
      covering lines `[a, b]` produces `(b - a + 1)` overlays, one
      per line, each with start = line-bol and end = line-eol of
      its line
- [ ] 6.2 Failing test: focus overlay on a 10-char line ends at
      column 10 in a 100-col window — i.e. `:extend` is not set on
      `+presentation-focus-face`
- [ ] 6.3 Failing test: focus overlays are deleted by
      `+presentation--cleanup-render-state` on slide leave
- [ ] 6.4 Failing test: a slide with focus AND annotations still
      places annotations correctly (no interference between the two
      overlay groups)
- [ ] 6.5 Replace the single-region overlay with the per-line
      implementation using `+presentation-focus-face`; tests green

## 7. MCP boundary & coercion

- [ ] 7.1 Failing test: snake_case → kebab-case coercion handles
      `kind` → `:kind`, `severity` → `:severity` on annotation
      plists at the MCP boundary
- [ ] 7.2 Failing test: an MCP payload with mixed-kind annotations
      on a single slide validates and renders correctly
- [ ] 7.3 Implement coercion extension; tests green
- [ ] 7.4 Update `push_slide` / `replace_slide` tool descriptions
      in `init.el` to mention the new `kind` / `severity` /
      `position` semantics with one-line examples

## 8. Documentation & polish

- [ ] 8.1 Update `modules/presentation/spec.md`: document
      annotation kinds, severities, position semantics per kind,
      the new focus-face behaviour
- [ ] 8.2 Run `make test`; fix byte-compile and checkdoc warnings
- [ ] 8.3 `openspec validate fix-presentation-overlay-rendering --strict`
- [ ] 8.4 Manual visual check: re-run the dogfood walkthrough and
      confirm the same slide layouts that were confusing now read
      cleanly
