# org-queue

Org-mode task queue management with SRS integration, numeric priorities, and smart scheduling.

## Overview

org-queue manages a unified task queue from your Org files. It collects both regular tasks and SRS (spaced repetition) items, interleaves them by a configurable ratio, and presents them one at a time for focused work.

**Key concepts:**

- **Outstanding**: Tasks available now (`available-at <= now`)
- **Pending**: Tasks due later today, auto-promoted when their time arrives
- **Two pools**: Non-SRS and SRS items merged at configurable ratio (default 1:4)
- **Night shift**: Suppresses SRS items during night hours

## Installation

```elisp
(add-to-list 'load-path "/path/to/org-queue")
(require 'org-queue)

;; Required: directory to scan for .org files (recursive)
(setq org-queue-directory "~/org-queue")
```

org-queue scans `.org` files recursively under `org-queue-directory`. It does not use or modify `org-agenda-files`.

## Module Structure

| File | Purpose |
|------|---------|
| `org-queue.el` | Entry point, loads all modules |
| `org-queue-config.el` | Configuration variables, defcustom definitions |
| `org-queue-utils.el` | Shared utilities, batched-save macros |
| `org-queue-tasks.el` | Queue management, outstanding/pending lists, show-top |
| `org-queue-display.el` | UI rendering, custom syntax highlighting, Anki integration |
| `org-queue-priority.el` | Numeric priority system (1-64), priority ranges (0-9) |
| `org-queue-schedule.el` | Scheduling with invariants, advance/postpone |
| `org-queue-srs-bridge.el` | org-srs integration, rating commands |
| `org-queue-id-guard.el` | ID resolution with timeout, auto-drop unresolved |
| `org-queue-chooser.el` | Tabulated list UI for browsing/bulk operations |
| `org-queue-keys.el` | Global keybindings (prefix `C-;`) |
| `org-queue-gptel-bridge.el` | gptel integration (launches Anki after AI requests) |

## Keybindings

Default prefix: `C-;`

### Core Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `c` | `org-queue-save-and-show-top` | Show queue head, save modified buffers |
| `u` | `org-show-parent-heading-cleanly` | Show parent heading context |
| `w` | `widen-and-recenter` | Widen buffer and recenter |

### Scheduling & Priority

| Key | Command | Description |
|-----|---------|-------------|
| `s` | `org-queue-schedule-and-prioritize` | Prompt months → schedule → prompt priority range → set priority → stamp LAST_REPEAT |
| `,` | `org-queue-prioritize-and-stamp` | Prompt priority range → set priority → stamp LAST_REPEAT |
| `+` | `org-queue-increase-priority-range-and-stamp` | Move to higher priority range → stamp |
| `-` | `org-queue-decrease-priority-range-and-stamp` | Move to lower priority range → stamp |
| `a` | `org-queue-advance-schedule-and-stamp` | Advance schedule earlier → stamp |
| `p` | `org-queue-postpone-schedule-and-stamp` | Postpone schedule later → stamp |

### SRS (Spaced Repetition)

| Key | Command | Description |
|-----|---------|-------------|
| `1` | `org-queue-srs-rate-again-and-prioritize` | Rate as "again" → prompt priority |
| `3` | `org-queue-srs-rate-good-and-prioritize` | Rate as "good" → prompt priority |
| `S` | `org-queue-srs-item-create-card-and-clean-stamp` | Create SRS card, remove LAST_REPEAT |

### Cloze & Extract

| Key | Command | Description |
|-----|---------|-------------|
| `z` | `org-queue-cloze-and-stamp` | Create cloze deletion (both directions) |
| `Z` | `org-queue-cloze-prefix-and-stamp` | Create cloze (prefix only) |
| `M-z` | `org-queue-cloze-suffix-and-stamp` | Create cloze (suffix only) |
| `x` | `org-queue-extract-and-stamp` | Create extract |
| `X` | `org-queue-remove-all-extracts-and-stamp` | Remove all extracts |

### Other

| Key | Command | Description |
|-----|---------|-------------|
| `A` | `my-launch-anki` | Launch or focus Anki |
| `D` | `org-demote-subtree` | Demote subtree |
| `P` | `org-promote-subtree` | Promote subtree |
| `l` | `org-web-tools-insert-link-for-url` | Insert link (if org-web-tools available) |
| `I` | `org-web-tools-insert-web-page-as-entry` | Insert web page (if org-web-tools available) |

## Priority System

org-queue uses numeric priorities from 1 (highest) to 64 (lowest).

### Priority Ranges

For ergonomic selection, priorities are grouped into 10 ranges (0-9):

```elisp
(setq my-priority-ranges
      '((0 . (1 . 2))    ; Range 0: priority 1-2 (urgent)
        (1 . (2 . 5))    ; Range 1: priority 2-5
        (2 . (5 . 12))   ; Range 2: priority 5-12
        (3 . (12 . 18))  ; Range 3: priority 12-18
        (4 . (18 . 24))  ; Range 4: priority 18-24
        (5 . (24 . 30))  ; Range 5: priority 24-30
        (6 . (30 . 37))  ; Range 6: priority 30-37
        (7 . (37 . 45))  ; Range 7: priority 37-45
        (8 . (45 . 58))  ; Range 8: priority 45-58
        (9 . (58 . 64)))) ; Range 9: priority 58-64 (low)
```

When prompted for priority, enter a range number (0-9). A random priority within that range is assigned.

### Priority Storage

Priority is stored in two places:
- `PRIORITY` property in the entry
- `[#N]` cookie in the heading line (e.g., `* [#15] My Task`)

## Scheduling System

### Invariants

All scheduling goes through a single path with these rules:

1. **Work window**: Snap to 09:00-18:00 local time
2. **Jitter**: Add ±10 minute randomization (within window)
3. **LAST_REPEAT deferral**: Non-SRS items respect `f(p) = base + slope * priority` minutes after last repeat
4. **Never past**: Never schedule into the past

### Scheduling Functions

- `my-random-schedule MONTHS`: Power-law distribution over MONTHS, biased by priority
- `my-advance-schedule`: Move schedule earlier (priority-aware)
- `my-postpone-schedule`: Move schedule later (priority-aware)

## Availability Calculation

### Non-SRS Items

```
available-at = max(SCHEDULED, LAST_REPEAT + base + slope * priority)
```

Configuration:
```elisp
(setq org-queue-non-srs-snooze-base-minutes 0)   ; Base deferral
(setq org-queue-non-srs-snooze-slope-minutes 10) ; Per-priority-point deferral
```

### SRS Items

`available-at` = next due time from the `:SRSITEMS:` drawer.

### QFORCE Property

Items with `QFORCE` property are treated as "today" even if scheduled in the future.

```elisp
(setq org-queue-qforce-ignores-last-repeat nil)  ; If t, QFORCE ignores LAST_REPEAT
```

## Night Shift

During night hours, SRS items are suppressed from the queue head (kept in pending).

```elisp
(setq org-queue-night-shift-enabled t)
(setq org-queue-night-shift-start "22:00")
(setq org-queue-night-shift-end "06:00")
```

## Queue Interleaving

Non-SRS and SRS items are interleaved at the head by ratio.

```elisp
(setq org-queue-srs-mix-ratio '(1 . 4))  ; 1 non-SRS per 4 SRS
```

Start policy determines which pool begins:

```elisp
(setq org-queue-mix-start 'rotate)  ; Options: 'non-srs 'srs 'rotate 'auto
```

## SRS Integration

org-queue integrates with `org-srs` for spaced repetition.

### SRS Concealment

When visiting SRS entries, the Back/Answer content is hidden by default:

```elisp
(setq org-queue-srs-conceal-answer t)  ; Set to nil to show answers
```

### Rating

- `org-queue-srs-rate-again`: Rate as "again" (failed recall)
- `org-queue-srs-rate-good`: Rate as "good" (successful recall)

## Custom Syntax Markers

org-queue supports special markers for cloze deletions and extracts:

```
{{type#ID|PAYLOAD|ID}}
```

Types:
- `cloze`: Visible placeholder on Front
- `clozed`: Hidden content (answer)
- `extract`: Extracted content

These are rendered with custom faces and the wrapper syntax is hidden.

## ID Guard

org-queue uses org-id for robust task tracking. The ID guard provides:

- **Timeout**: ID resolution is time-boxed to 0.12 seconds
- **Cooldown**: Failed IDs are cached for 300 seconds before retry
- **TRAMP skip**: Remote files are never resolved
- **Auto-drop**: Unresolved entries are automatically removed during maintenance

## Chooser UI

`org-queue-chooser-mode` provides a tabulated list interface:

| Key | Action |
|-----|--------|
| `j`/`k` | Move down/up |
| `RET`/`f` | Visit entry |
| `m`/`u` | Mark/unmark |
| `t` | Toggle mark |
| `U` | Unmark all |
| `S` | Spread schedule across marked |
| `H` | Spread priorities across marked |
| `a`/`p` | Advance/postpone marked |
| `g`/`G` | Refresh / hard refresh |
| `q` | Quit |

## Configuration Reference

```elisp
;; Required
(setq org-queue-directory "~/org-queue")

;; Night shift
(setq org-queue-night-shift-enabled t)
(setq org-queue-night-shift-start "22:00")
(setq org-queue-night-shift-end "06:00")

;; Interleaving
(setq org-queue-srs-mix-ratio '(1 . 4))
(setq org-queue-mix-start 'rotate)

;; Non-SRS deferral
(setq org-queue-non-srs-snooze-base-minutes 0)
(setq org-queue-non-srs-snooze-slope-minutes 10)

;; QFORCE behavior
(setq org-queue-qforce-ignores-last-repeat nil)

;; SRS
(setq org-queue-srs-conceal-answer t)

;; Auto-show after changes
(setq org-queue-auto-show-top-after-change nil)

;; Verbose messages
(setq org-queue-verbose t)

;; Visible buffer count
(setq my-queue-visible-buffer-count 8)
```

## Anki Integration

`my-launch-anki` launches or focuses Anki on macOS, Linux, and Windows:

- **macOS**: Uses `osascript` and `open`
- **Linux**: Uses `wmctrl` or `xdotool` if available
- **Windows**: Uses PowerShell
- **Android**: Prints hint to open manually

Suppression for current session:
```
M-x org-queue-toggle-anki-suppression
```

## GPTel Bridge

When `gptel` is installed, sending a request automatically launches Anki (daytime only). This helps maintain SRS habits while using AI assistance.

## Maintenance

`org-queue-maintenance` performs:

- Ensure priorities/schedules for all headings
- Advance near-future schedules
- Postpone overdue TODOs
- Clean DONE tasks (remove SCHEDULED/PRIORITY)
- Drop unresolved ID entries

A midnight timer rebuilds queues and runs maintenance daily.

## File Caching

org-queue caches the file list daily in `.org-queue-files-cache.el` within `org-queue-directory`. This makes startup faster and enables portability across machines with the same org-queue tree.

## License

See LICENSE file.
