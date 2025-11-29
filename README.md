# org-queue — Task queue management for Org mode

A tightly integrated queue for Org mode that blends **scheduling**, **numeric priority control (1–64)**, and **SRS reviews** into fast, low‑churn workflows. The design emphasizes a **single-pass queue build**, **deterministic interleaving**, **minimal I/O (single-save batching)**, and **robust ID guarding**.

---

## Overview

- **Two-pool head, built in one pass**
  - Non‑SRS and SRS items are discovered across your Org files in a **single pass** and merged into one queue.
  - The head of the queue is **interleaved by ratio** `(non‑SRS . SRS)`, default **`(1 . 4)`**, with a configurable start policy.
- **Outstanding vs. Pending**
  - **Outstanding**: `available‑at ≤ now` (immediately actionable).
  - **Pending (today)**: due **later today**. They auto‑promote when they become available (on show‑top; optional timer).
- **Night shift**
  - During night shift (local default **22:00–06:00**), **SRS items are suppressed** from the head. Non‑SRS still flow.
- **Deterministic prompts**
  - Combined flows prompt for **months** (scheduling) and a **priority range (0–9)**, then write a **numeric priority**.
- **Single-save batching**
  - Heavier combo commands coalesce all edits into **one save** to reduce editor churn.
- **Micro-updates**
  - Changes to SCHEDULED or PRIORITY are tracked and **coalesced** (idle) into micro‑updates + autosave.
- **ID guard**
  - ID resolution is **time-boxed** (0.12s), skips TRAMP files, and auto‑drops unresolved queue entries.
- **SRS concealment**
  - Hides the **Back/Answer** content when visiting SRS entries (Front visible), with an option to disable.
- **Custom syntax helpers**
  - **Cloze** and **Extract** creation helpers using `{{type#ID|...|ID}}` markers with live highlighting.
- **Anki & GPTel integration**
  - Cross‑platform **Anki launcher**; an optional GPTel bridge can auto‑focus/launch Anki after you send a request (daytime only).

If you already use Org for planning and SRS for memory, `org-queue` helps you work the day’s items in a consistent, ergonomic order with minimal friction.

---

## Installation

1. Put all `org-queue-*.el` files on your `load-path`, then in your init:

   ```elisp
   (require 'org-queue)

   ;; Base directory for recursive .org indexing (required)
   (setq org-queue-directory "~/org-queue")

   ;; Start up automatically (installed via emacs-startup-hook by default)
   ;; To start manually instead, remove the hook after require:
   ;; (remove-hook 'emacs-startup-hook #'org-queue-startup)
   ;; and call (org-queue-startup) yourself when desired.
   ```

2. (Optional) If you want a different global prefix than the default **`C-;`**, rebind the prefix map **after** `(require 'org-queue)`:

   ```elisp
   (global-set-key (kbd "C-c q") org-queue-prefix-map)  ;; example
   ```

> `org-queue` scans `.org` files **recursively** under `org-queue-directory`. It **does not** read or modify `org-agenda-files`.

---

## Keybindings (global prefix: `C-;`)

The global prefix map is installed in **`org-queue-keys.el`** and bound to `C-;` by default.

| Key | Command | Purpose |
|---:|:---|:---|
| `c` | `org-queue-show-top` | Show the current queue head (promotes items that became due). |
| `s` | `org-queue-schedule-and-prioritize` | Prompt **months** → schedule (with invariants) → prompt **priority range (0–9)** → write numeric priority → **single save**. |
| `,` | `org-queue-prioritize-and-stamp` | Prompt **priority range (0–9)** → set numeric priority → stamp `:LAST_REPEAT:` (non‑SRS) → **single save**. |
| `+` / `-` | `org-queue-increase-priority-range-and-stamp` / `org-queue-decrease-priority-range-and-stamp` | Shift the active range then set a numeric priority in it → stamp (non‑SRS) → **single save**. |
| `a` / `p` | `org-queue-advance-schedule-and-stamp` / `org-queue-postpone-schedule-and-stamp` | Move SCHEDULED forward/back using math- and priority‑aware heuristics → stamp (non‑SRS) → **single save**. |
| `1` / `3` | `org-queue-srs-rate-again-and-prioritize` / `org-queue-srs-rate-good-and-prioritize` | Rate SRS (**again / good**) → prompt **priority range** → numeric priority → **single save**. |
| `A` | `my-launch-anki` | Focus/launch Anki (night shift disables auto‑launch). |
| `x` / `X` | `org-queue-extract-and-stamp` / `org-queue-remove-all-extracts-and-stamp` | Create Extract or remove all Extract markers → stamp (non‑SRS) → **single save**. |
| `z` / `Z` / `M-z` | `org-queue-cloze-and-stamp` / `org-queue-cloze-prefix-and-stamp` / `org-queue-cloze-suffix-and-stamp` | Create Cloze (both / prefix / suffix) → stamp (non‑SRS) → **single save**. |
| `u` / `w` | `org-show-parent-heading-cleanly` / `widen-and-recenter` | Navigation helpers for context. |
| `D` / `P` | `org-demote-subtree` / `org-promote-subtree` | Structure editing helpers. |
| `S` | `org-queue-srs-item-create-card-and-clean-stamp` | Convert current non‑SRS entry to an SRS card (removes `:LAST_REPEAT:` if present) → **single save**. |

**Optional keys (present when packages are available):**

- If `org-web-tools` is loaded:
  `l` → `org-web-tools-insert-link-for-url`, `I` → `org-web-tools-insert-web-page-as-entry`.

---

## Everyday flows

### 1) Schedule **and** prioritize (recommended)

**`C-; s` → `org-queue-schedule-and-prioritize`**

1. Prompts for **months** and schedules through the canonical path:
   - Avoid **weekends** (push to Monday)
   - Snap into the local **work window** (default `[09:00, 18:00)`)
   - Add ±10min **jitter** (kept inside the window)
   - Respect **priority‑based deferral** over `:LAST_REPEAT:` (non‑SRS only)
2. Prompts for a **priority range (0–9)** and writes a **numeric priority (1–64)** to the property and `[#N]` cookie.
3. Runs as a **single-save** batch and performs a final `'stamp` micro‑update if stamping occurred.

### 2) Review (SRS) and prioritize

**`C-; 1`** (again) or **`C-; 3`** (good)

- Rates via `org-srs` and advances the **interleave phase** once (to keep head rotation consistent).
- Then **prompts for a priority range** → writes a numeric priority → **single save**.
- Finishes with a `'priority` micro‑update so the queue reflects the latest state.

---

## Queue model

### Two pools, one head

- Build **non‑SRS** and **SRS** items in one pass across your files.
- Interleave by **ratio** at the head. Default: `(1 . 4)` → 1 non‑SRS per 4 SRS in the head block (daytime).
  During **night shift**, SRS are **kept pending** and not interleaved.

### Start policy

- `org-queue-mix-start` controls which pool starts the head:
  - `'non-srs` | `'srs` | `'rotate` (default; tracked by a phase variable) | `'auto` (earlier due item wins).

### Outstanding vs. Pending (today)

- **Outstanding**: tasks with `available‑at ≤ now`.
- **Pending (today)**: tasks due later today. They are auto‑promoted when they become available:
  - Always promoted when you call `org-queue-show-top`.
  - (Optional) Periodic promotions via `org-queue-start-pending-promotion-timer` (60s interval).

---

## How availability is computed

### SRS items

- `available‑at` is the **next due time** parsed from the SRS drawer (default drawer name: `:SRSITEMS:`).
- The **Front** is shown; **Back/Answer** content is concealed when `org-queue-srs-conceal-answer` is non‑nil.

### Non‑SRS items

- `available‑at = max(SCHEDULED, LAST_REPEAT + f(priority))` where:

  ```elisp
  f(p) = org-queue-non-srs-snooze-base-minutes
       + org-queue-non-srs-snooze-slope-minutes * p
  ```

- **QFORCE** (property named by `org-queue-force-outstanding-property`, default `"QFORCE"`) marks an item as “today” even if scheduled in the future.
  - When `org-queue-qforce-ignores-last-repeat` is non‑nil, QFORCE makes items **available immediately** (ignores `:LAST_REPEAT:`).

---

## Priority model (numeric 1–64)

- `org-queue` uses **numeric priorities**: highest **1**, default **32**, lowest **64**.
- The value is written to the **`PRIORITY` property** and mirrored in the heading as a **`[#N]` cookie**.
- Priority ranges are grouped (0–9) for fast, ergonomic selection:

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

Commands:

- `my-set-priority-with-heuristics` — prompt for range 0–9 (defaults to current range if any), then set a numeric priority in it.
- `my-increase-priority-range` / `my-decrease-priority-range` — shift the active range and set a new numeric priority.
- `my-ensure-priority-set` — ensures the current heading has a valid numeric priority (skips SRS parent entries).

---

## Scheduling invariants & helpers

Always-on rules (single canonical path):

- Avoid **weekends** (push to next Monday).
- Snap into **local work window** `[09:00, 18:00)`.
- Apply a small **±10-minute jitter** kept inside the window.
- Respect **`LAST_REPEAT` deferral** (non‑SRS) using the priority-based function `f(p)` above.

Interactive helpers:

- `my-find-schedule-weight` — derive a month “weight” from current SCHEDULED date (past/today → 0; no schedule → default).
- `my-random-schedule MONTHS &optional n` — power‑law distribution across `MONTHS` with **priority‑aware bias** (earlier for higher priority).
- `my-advance-schedule` / `my-postpone-schedule` — continuously adjusted by priority and current schedule weight; never schedules into the past.
- `org-queue-stamp-last-repeat-current` — stamps `:LAST_REPEAT:` on non‑SRS entries only (SRS and SRS-parents are skipped safely).

---

## SRS integration

- `org-srs` is optional; functions are gated to degrade gracefully when absent.
- Rating commands (for muscle memory):
  `org-queue-srs-rate-again` / `org-queue-srs-rate-good` and their **…-and-prioritize** variants.
- Creating cards:
  `org-queue-srs-item-create-card` and `org-queue-srs-item-create-card-and-clean-stamp` (removes `:LAST_REPEAT:` if present, then creates a card).

**Concealment:**

- `org-queue-srs-conceal-answer` (default `t`) hides Back/Answer bodies. Change with:

  ```elisp
  (setq org-queue-srs-conceal-answer t)  ;; or nil
  ```

---

## Cloze / Extract and custom syntax markers

- **Markers:** `{{type#ID|PAYLOAD|ID}}`, where `type` ∈ `{cloze, clozed, extract}`.
- **Cloze:**
  - `org-interactive-cloze` (and `…-prefix`, `…-suffix`) creates:
    - `{{clozed#ID|…|ID}}` in place of selection.
    - Front on child heading(s) using `{{cloze#ID|[…]|ID}}` (ellipsis configurable via `org-interactive-cloze-ellipsis`).
  - Front content unwraps other cloze markers for clean display.
- **Extract:**
  `org-interactive-extract` creates `{{extract#ID|…|ID}}` and a child with the cleaned payload.
- **Rendering:**
  Automatic highlighting hides wrappers and styles payload (nested markers supported).
  Toggle visibility: `org-toggle-syntax-markers`.

---

## Maintenance & safety

- **Daily Maintenance** (`org-queue-maintenance`) — a guarded, batched pipeline that:
  - Ensures missing priorities/schedules (skips SRS scheduling; skips DONE),
  - Advances some near‑future schedules (power set; default `2^8`),
  - Auto‑postpones overdue TODOs,
  - Postpones duplicate outstanding priorities per file,
  - Enforces **monotone cap** (“lower priorities never outnumber any higher priority”),
  - Removes consecutive same‑file head duplicates (keep first),
  - Cleans DONE tasks (removes SCHEDULED/PRIORITY),
  - Rebuilds queues once at the end.
- **ID Guard** (always on):
  - Time‑boxed ID resolution (**0.12s**), **300s cooldown** on failures,
  - Skips TRAMP files,
  - Auto‑drops unresolved items during maintenance, midnight refresh, and visit failures,
  - After saves, re‑resolves file‑local markers on idle in small batches.
- **Autosave control:**
  - Heavy operations run under **single-save batching** macros.
  - Micro‑updates (schedule/priority edits) are idle‑coalesced and autosaved.

**Automatic scheduling:**

- On Emacs startup, `org-queue-startup` builds the queue and shows the head (hooked by default).
- Midnight refresh reschedules itself daily, clears today’s pending, rebuilds queues, and triggers daily maintenance (if needed).

---

## Configuration (minimal)

```elisp
;; Where to scan .org files (recursive)
(setq org-queue-directory "~/org-queue")

;; Night shift (suppresses SRS at head)
(setq org-queue-night-shift-enabled t)
(setq org-queue-night-shift-start "22:00")
(setq org-queue-night-shift-end   "06:00")

;; Interleave ratio and start policy
(setq org-queue-srs-mix-ratio '(1 . 4))
(setq org-queue-mix-start 'rotate)  ;; 'non-srs | 'srs | 'rotate | 'auto

;; Non‑SRS deferral tuning (minutes)
(setq org-queue-non-srs-snooze-base-minutes 0)
(setq org-queue-non-srs-snooze-slope-minutes 1)

;; QFORCE policy
(setq org-queue-qforce-ignores-last-repeat nil)

;; Optional: automatically show queue head after edits
(setq org-queue-auto-show-top-after-change nil)
```

**Notes & defaults (from code):**

- Numeric priorities are configured globally:

  ```elisp
  (setq org-priority-highest 1
        org-priority-default 32
        org-priority-lowest  64)
  ```

- File roster cache TTL: `org-queue-file-cache-ttl` (seconds, default **10**).
- Cache dir (best-effort) under `var/org-queue/` respecting `no-littering` if present.

---

## Optional integrations

- **Anki launcher** (`my-launch-anki`): macOS (`osascript`), Linux (`wmctrl`/`xdotool` if available), Windows (PowerShell). Android branch prints a hint (open manually). Disabled during night shift.
- **GPTel bridge** (`org-queue-gptel-bridge.el`): after `gptel-send`, launches/focuses Anki (daytime only).
- **org-web-tools**: optional keybindings `l` / `I` under the prefix.
- **org-roam**: an optional, one‑shot DB sync step is included in maintenance if present.

---

## Tips

- Prefer the **combined flows** for consistent invariants and single-save batching:
  - `C-; s` for schedule+priority (non‑SRS),
  - `C-; 1` / `C-; 3` for rate+priority (SRS).
- Want just “raw schedule” with the same math but without the combo prompt? Use:

  ```elisp
  (call-interactively 'my-random-schedule-command)
  ```

- To keep the head clean, rely on the built‑in pruning:
  - Night shift suppresses SRS,
  - `org-queue-show-top` prunes not‑due items at the head immediately.

---

## Troubleshooting

- **No effect / no prompts?** Ensure point is on an **Org heading** and the package is loaded.
- **SRS items not appearing at the head?** You may be in **night shift**. They will still be collected as **pending**.
- **Unresolved or stale IDs?** The **ID guard** auto‑drops unresolved items (time‑boxed resolution; TRAMP skipped). Maintenance and midnight refresh also clean these up.
- **Too many SRS in the head?** Adjust `org-queue-srs-mix-ratio` and/or `org-queue-mix-start`.
- **Queue not updating after edits?** Micro‑updates run on idle; you can also call `org-queue-show-top` to force a refresh + promotion.

---

## License

See the project’s main license file.
