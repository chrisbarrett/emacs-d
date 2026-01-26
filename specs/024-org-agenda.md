# org-agenda

Org agenda views and habit tracking configuration.

## Source Files

| File                    | Purpose                       |
| :---------------------- | :---------------------------- |
| `config/mod-org-agenda.el` | Agenda views, habit config |
| `lisp/+agenda.el`       | Skip functions and predicates |
| `scripts/update-agenda-files.sh` | Auto-scan for agenda files |

## External Packages

| Package            | Purpose                    |
| :----------------- | :------------------------- |
| `org-habit`        | Habit tracking with graphs |
| `org-super-agenda` | Agenda section grouping    |
| `page-break-lines` | Visual block separators    |

## Behaviors

### Agenda Files

| Behavior                   | Setting/Implementation                      |
| :------------------------- | :------------------------------------------ |
| Files list location        | `org-directory/org-agenda-files`            |
| Text search includes       | `agenda-archives`, `archive.org`            |
| Auto-scan on save          | `+org-agenda-update-files` via after-save   |
| Scan method                | ripgrep for `CLOCK:` or TODO/WAIT headlines |
| Skip unavailable files     | `org-agenda-skip-unavailable-files t`       |

### View Configuration

| Behavior                    | Setting/Value                     |
| :-------------------------- | :-------------------------------- |
| Default span                | `'day`                            |
| Start day                   | Today (`nil`)                     |
| Start on weekday            | Disabled (`nil`)                  |
| Window setup                | `'only-window`                    |
| Restore windows after quit  | Enabled                           |
| Show future repeats         | Disabled                          |
| Include diary               | Disabled                          |
| Show inherited tags         | Disabled                          |
| Tags column                 | 0 (immediate after heading)       |
| Dim blocked tasks           | Enabled                           |
| Time grid                   | Disabled globally, enabled in agenda view |

### Skip Logic

| Behavior                         | Setting/Implementation               |
| :------------------------------- | :----------------------------------- |
| Skip scheduled if done           | Enabled                              |
| Skip deadline if done            | Enabled                              |
| Skip scheduled if deadline shown | Enabled                              |
| Skip deadline prewarning         | `'pre-scheduled`                     |
| Ignore scheduled future in todo  | `'future`                            |
| Ignore properties                | `'(effort appt)`                     |

### Sorting Strategy

| View     | Strategy                                          |
| :------- | :------------------------------------------------ |
| Agenda   | time-up, habit-up, priority-down, category-up     |
| Todo     | priority-down, category-up, scheduled-up          |
| Tags     | priority-down, category-up                        |
| Search   | category-up                                       |

### Custom Commands

Two main views defined with identical structure but different tag filters:

| Key | Name            | Tag Filter              |
| :-- | :-------------- | :---------------------- |
| `p` | Personal agenda | `-work -ignore`         |
| `w` | Work agenda     | `-ignore +{work-tag}`   |

Each view contains these sections:

| Section        | Query                          | Super-agenda Groups                            |
| :------------- | :----------------------------- | :--------------------------------------------- |
| Agenda         | `agenda ""`                    | time-grid, Forming, French, Cooking, Chores, Habits, Birthdays, Delegated, Tickler |
| Next Actions   | `tags-todo -tickler-inbox+TODO="TODO"` | High-priority, first TODO at level    |
| Inbox          | `tags-todo +inbox+TODO="TODO"` | Unscheduled items                              |
| Delegated      | `todo "WAIT"`                  | Unscheduled wait items                         |
| Projects       | `tags-todo +TODO="PROJECT"`    | All projects                                   |

### Skip Functions

| Function                          | Logic                                    |
| :-------------------------------- | :--------------------------------------- |
| `+agenda-view-skip-function`      | Skip archived entries                    |
| `+agenda-next-actions-skip-function` | Skip scheduled/deadlined; include high-priority; first TODO at level only |

### Keybindings

| Key        | Command                       | Scope     |
| :--------- | :---------------------------- | :-------- |
| `C-x C-s`  | `org-save-all-org-buffers`    | motion    |
| `J`        | `org-agenda-goto-date`        | motion    |
| `C-n`      | `org-agenda-later`            | motion    |
| `C-p`      | `org-agenda-earlier`          | motion    |

### Follow Behavior

TAB in agenda reveals context:

1. `org-overview` - collapse all
2. `org-reveal` - show ancestors
3. `org-fold-show-subtree` - expand current tree
4. `org-display-outline-path` - show path in echo area

### Block Separator

| Behavior           | Implementation                        |
| :----------------- | :------------------------------------ |
| Separator char     | Form feed (`?\f`)                     |
| Display            | `page-break-lines-mode`               |
| Update triggers    | `org-agenda` and `org-agenda-redo`    |

## org-habit

### Settings

| Setting                   | Value |
| :------------------------ | :---- |
| `org-habit-graph-column`  | 72    |
| `org-habit-today-glyph`   | `!`   |
| `org-habit-completed-glyph` | `*` |

### Dynamic Graph Sizing

Graph width scales with window:

| Variable                        | Purpose                              |
| :------------------------------ | :----------------------------------- |
| `+org-habit-graph-window-ratio` | Graph width as fraction (default 0.2)|
| `+org-habit-graph-padding`      | End padding (default 2)              |
| `+org-habit-min-width`          | Hide below this width (default 30)   |

`+org-habit-resize-graph-h` recalculates on `org-agenda-mode-hook`.

### Clock Consistency Checks

| Check          | Value              |
| :------------- | :----------------- |
| Gap OK around  | 12:20, 12:40, 4:00 |
| Max duration   | 10:00              |
| Min duration   | 0                  |
| Max gap        | 0                  |

## Predicates (+agenda.el)

| Function                          | Returns t when                       |
| :-------------------------------- | :----------------------------------- |
| `+agenda--any-scheduled-or-deadline-p` | Entry has schedule or deadline  |
| `+agenda--scheduled-in-future-p`  | Schedule time is after now           |
| `+agenda--scheduled-now-p`        | Schedule time equals now             |
| `+agenda--at-TODO-p`              | Entry has TODO state                 |
| `+agenda--first-todo-at-this-level-p` | First TODO among siblings       |
| `+agenda--high-priority-p`        | Priority is A                        |
| `+agenda--parent-scheduled-in-future-p` | Any ancestor scheduled future  |

## Testable Properties

1. `org-agenda-files` points to file in `org-directory`
2. `org-agenda-span` is `'day`
3. Custom commands "p" and "w" are defined
4. `page-break-lines-modes` includes `org-agenda-mode`
5. Skip function excludes archived entries
6. Skip function includes high-priority TODOs regardless of sibling order
7. Habit graph column adjusts to window width
8. After-save hook updates agenda files in org-mode
9. TAB in agenda view runs reveal sequence
