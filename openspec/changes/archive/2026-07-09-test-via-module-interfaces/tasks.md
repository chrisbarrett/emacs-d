# Tasks: test-via-module-interfaces

## 1. Rewrite confirmed offenders

- [x] 1.1 `modules/evil/tests.el`: replace each source-substring test
      with a state assertion (evil-mode enabled; `evil-undo-system`
      value; cursor shape variables; hook membership); confirm the new
      tests fail if the state is wrong (mutate, observe red, restore)
- [x] 1.2 `modules/org-capture/tests.el`: same treatment for its
      `insert-file-contents` init.el assertions
- [x] 1.3 `modules/vulpea/tests.el`: same treatment

## 2. Audit remaining search-forward suites

- [x] 2.1 Audit `search-forward` usage in lang-elixir, lang-markdown,
      lang-rust, lang-shscript, lang-terraform, lang-zig, leader,
      org-agenda tests: classify each as fixture-search (keep) or
      module-source search (rewrite)
      — lang-*: all read `.eld` template fixtures (keep);
        org-agenda: scratch org buffer (keep);
        leader: reads own init.el/lib.el (rewrite)
- [x] 2.2 Rewrite any module-source searches found; drop assertions
      with no observable consequence, noting each in the commit message
      — leader 4 tests rewritten to state assertions

## 3. Close out

- [x] 3.1 Verify the spec scenario: no `insert-file-contents` of a
      module's own init.el/lib.el remains under `modules/*/tests.el`
- [x] 3.2 Full `make test` green; spot-check that a whitespace-only
      init.el edit in evil no longer fails its suite (then revert the
      edit)
