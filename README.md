# org-queue — Task queue management for Org mode

A tightly integrated queue for Org mode that blends **scheduling**, **priority control**, and **SRS reviews** into fast, low-churn workflows. The project emphasizes deterministic prompts, predictable ordering, and minimal I/O.

---

## Overview

- **Two-pool queue**: non‑SRS and SRS entries are built in one pass and **interleaved** at the head using a `(non‑SRS . SRS)` ratio.
- **Outstanding vs. Pending**: entries due **now** are outstanding; those due **later today** are pending and get promoted automatically when they become available.
- **Deterministic prompts**: combined flows always prompt for months (when scheduling) and a priority range (0–9).
- **Single-save batching**: combined flows coalesce all saves into **one** write to reduce editor churn.
- **Sensible invariants**: avoid weekends, respect work-window and small jitter, and apply **priority‑based deferral** over `LAST_REPEAT` for non‑SRS entries.
- **Night shift aware**: SRS items are suppressed from the head during night shift.

If you already use Org for planning and SRS for memory, `org-queue` helps you work the day’s items in a consistent, ergonomic order with minimal friction.

---

## Installation

1. Place all `org-queue-*.el` files somewhere on your `load-path`.
2. In your init:
   ```elisp
   (require 'org-queue)
   ;; optional: base directory for recursive .org indexing
   (setq org-queue-directory "~/org")
   ;; startup (can also rely on the included startup hook)
   (org-queue-startup)
   ```
3. (Optional) To have a different prefix than the default `C-;`, remap `org-queue-prefix-map` to your liking.

---

## Keybindings (global prefix: `C-;`)

| Key | Command | Purpose |
|---:|:---|:---|
| `s` | `org-queue-schedule-and-prioritize` | Prompt **months** → schedule with invariants → prompt **priority range (0–9)** → write numeric priority → **single save** |
| `1` | `org-queue-srs-rate-again-and-prioritize` | SRS **again** → prompt **priority range** → numeric priority → **single save** |
| `3` | `org-queue-srs-rate-good-and-prioritize` | SRS **good** → prompt **priority range** → numeric priority → **single save** |
| `,` | `my-set-priority-with-heuristics` | Interactively choose a **priority range**; writes a numeric priority |
| `+` / `-` | `my-increase-priority-range` / `my-decrease-priority-range` | Shift the active range and set a numeric priority in it |
| `a` / `p` | `my-advance-schedule` / `my-postpone-schedule` | Move SCHEDULED forward/back with math- and priority‑aware heuristics |
| `c` | `org-queue-stamp-and-show-top` | Stamp `:LAST_REPEAT:` (when allowed), then show queue head |
| `u` / `w` | `org-show-parent-heading-cleanly` / `widen-and-recenter` | Navigation helpers for context |
| `z` / `Z` / `M-z` | `org-interactive-cloze` variants | Create cloze deletions and related child entries |
| `X` | `org-remove-all-extract-blocks` | Cleanup helper for `{{extract#...}}` blocks |

> The prefix map is installed globally in `org-queue-keys.el`. You can rebind any entry after `(require 'org-queue)`.

---

## Everyday flows

### Schedule and prioritize **(recommended)**
**`C-; s` → `org-queue-schedule-and-prioritize`**

1. Prompts for **months** and schedules through the canonical path:
   - Avoid weekends (push to Monday)
   - Snap into the local **work window** (default `[09:00, 18:00)`)
   - Add a small ±10min **jitter** inside the window
   - Respect **priority-based deferral** over `:LAST_REPEAT:` for non‑SRS entries
2. Prompts for a **priority range (0–9)** and writes a numeric priority inside the chosen range (property + `[#[N]]` cookie).
3. Uses **single-save batching**. Ends with a final micro-update tagged `'priority`.

### Review and prioritize (SRS)
**`C-; 1`** (again) or **`C-; 3`** (good)

- First performs the rating (emitting its own `'review` micro-update).
- Then **prompts for a priority range**, writes a numeric priority, and coalesces to **one** save.
- Finishes with a `'priority` micro-update so the queue reflects the latest state.

---

## Queue model

### Two pools, one head
- Build non‑SRS and SRS items in a single pass.
- Interleave by ratio at the head. Example default: `(1 . 4)` → 1 non‑SRS per 4 SRS in the head block.
- Head-start selection can be **`non-srs`**, **`srs`**, **`rotate`**, or **`auto`**. Rotation advances as you consume the head; `auto` prefers the pool with the item due earlier.

### Outstanding vs. Pending
- **Outstanding**: items with `available‑at ≤ now` are immediately actionable.
- **Pending (today)**: items due later today. Auto-promoted by a periodic timer, and also promoted when you call `org-queue-show-top`.
- **Night shift**: SRS items are kept pending and not interleaved into the head; non‑SRS still flow in.

### How availability is computed
- **SRS**: from the next due time parsed from the SRS drawer (e.g., `:SRSITEMS:`).
- **Non‑SRS**: by default `available‑at = max(SCHEDULED, LAST_REPEAT + f(priority))`, where:
  ```elisp
  f(p) = org-queue-non-srs-snooze-base-minutes + org-queue-non-srs-snooze-slope-minutes * p
  ```
- **QFORCE**: set property named by `org-queue-force-outstanding-property` to treat an item as outstanding today. When `org-queue-qforce-ignores-last-repeat` is non‑nil, QFORCE makes items **available immediately** regardless of `:LAST_REPEAT:`.

---

## Configuration

### Minimal setup
```elisp
(setq org-queue-directory "~/org")         ;; recursive .org scan root
(setq org-queue-night-shift-enabled t)     ;; respect night shift window
(setq org-queue-night-shift-start "22:00")
(setq org-queue-night-shift-end   "06:00")

;; Interleave ratio and start policy
(setq org-queue-srs-mix-ratio '(1 . 4))
(setq org-queue-mix-start 'rotate)         ;; 'non-srs | 'srs | 'rotate | 'auto

;; Non‑SRS deferral tuning (minutes)
(setq org-queue-non-srs-snooze-base-minutes 0)
(setq org-queue-non-srs-snooze-slope-minutes 1)

;; QFORCE policy
(setq org-queue-qforce-ignores-last-repeat nil)
```

### Priority ranges (example)
```elisp
(setq my-priority-ranges
      '((0 . (1 . 2))
        (1 . (2 . 5))
        (2 . (5 . 12))
        (3 . (12 . 18))
        (4 . (18 . 24))
        (5 . (24 . 30))
        (6 . (30 . 37))
        (7 . (37 . 45))
        (8 . (45 . 58))
        (9 . (58 . 64))))
```

---

## Anki & SRS notes

- `my-launch-anki` focuses or launches Anki where possible (macOS uses AppleScript; Linux may use `wmctrl`/`xdotool` if available; Windows uses PowerShell). On Android, open the app manually.
- If `gptel` is present, sending a request can trigger an Anki launch unless you are in **night shift**.
- SRS answer concealment: when visiting SRS entries, the **Back/Answer** body is hidden so you can review the Front cleanly.

---

## Maintenance & safety

- **Daily maintenance** (`org-queue-maintenance`): a guarded, batched pipeline to normalize priorities/schedules, nudge near‑future schedules forward, reduce noise (overdue/postponed), and tidy duplicates. Runs via a stamp file gate on startup (configurable).
- **ID guard**: time‑boxes ID resolution to avoid hangs, skips remote/TRAMP files, tracks recent failures (cooldown), and re‑resolves file‑local markers after saves on idle.
- **Micro‑updates**: edits to SCHEDULED or PRIORITY coalesce into short‑delay updates so the queue stays in sync without thrashing.
- **Single‑save macros**: batch heavier operations via `org-queue--with-batched-saves`.

> The maintenance pipeline is opinionated; read it before enabling in environments where aggressive rescheduling is undesirable.

---

## Tips

- Want raw schedule **without** prompting for a range? Use your original `my-schedule-command` (bind it elsewhere, e.g., `C-; S`).  
- Prefer separate saves for debugging? Temporarily remove the batching macro in your local copy of the combo commands.  
- Night work? Set `org-queue-night-shift-enabled` to `nil` or adjust its window to surface SRS items immediately.

---

## Troubleshooting

- **No prompts?** Ensure point is on an Org **heading** and that `org-queue` is loaded.  
- **SRS items missing?** You may be in **night shift**, or entries may not have a recognized SRS drawer.  
- **Duplicate entries or stale IDs?** The ID guard drops unresolved items and re‑resolves file‑local markers on idle after saves.  
- **Too many SRS in the head?** Adjust `org-queue-srs-mix-ratio` and/or `org-queue-mix-start`.

---

## License
See the project’s main license file.
