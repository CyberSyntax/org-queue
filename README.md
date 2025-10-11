# Org‑queue

⚠️ **WARNING: USE WITH CAUTION** ⚠️  
`org-queue` programmatically edits your Org files (scheduling, properties, IDs, etc.). Always keep backups, review the source, and understand the workflows before enabling automatic maintenance.

---

**Tagline:** File‑first, two‑queue, SRS‑aware incremental task engine for Org mode — with strict scheduling invariants, night‑shift controls, live micro‑updates, and ID guarding.

- Works directly over a directory of `.org` files (no agenda dependency).
- Maintains an **Outstanding** queue (available now) and a **Pending** queue (available later today).
- Integrates **SRS items** (from an `:SRSITEMS:` drawer) with intelligent mixing and concealment.
- Enforces **file‑first scheduling invariants** (weekday-only, work window, jitter).
- Live **micro‑updates** on schedule/priority edits (no manual rebuilds needed).
- Robust **ID guard** prevents hangs on bad/missing IDs and silently drops unrecoverable entries.

---

## Table of Contents

1. [Quick Start](#quick-start)  
2. [Key Concepts](#key-concepts)  
   - [Two‑Queue Architecture](#two-queue-architecture)  
   - [Scheduling Invariants](#scheduling-invariants)  
   - [SRS Integration](#srs-integration)  
   - [Night Shift](#night-shift)  
   - [ID Guard](#id-guard)
3. [Keybindings (Global Prefix: `C-;`)](#keybindings-global-prefix-c-)  
4. [Configuration](#configuration)  
   - [Essential](#essential)  
   - [Scheduling & Snooze](#scheduling--snooze)  
   - [SRS & Mixing](#srs--mixing)  
   - [Night Shift Window](#night-shift-window)  
   - [Performance & Files](#performance--files)
5. [Maintenance Pipeline](#maintenance-pipeline)  
6. [Custom Syntax: Extracts & Clozes](#custom-syntax-extracts--clozes)  
7. [Optional Integrations](#optional-integrations)  
8. [Migration Notes (Breaking Changes)](#migration-notes-breaking-changes)  
9. [Troubleshooting](#troubleshooting)  
10. [License](#license)

---

## Quick Start

### Prerequisites
- **Emacs** 27.1+
- **org-mode** 9.0+
- Optional: **org-srs**, **org-web-tools**, **gptel**, **org-roam** (maintenance hook)

### Install
```sh
git clone https://github.com/CyberSyntax/org-queue.git
```

### Minimal Config (init.el)
```emacs-lisp
;; Load path and base directory
(add-to-list 'load-path "~/path/to/org-queue")
(setq org-queue-directory "~/org")      ;; required for file discovery

(require 'org-queue)                     ;; loads all modules

;; Optional, but useful defaults
(setq my-queue-visible-buffer-count 8)
(setq my-random-schedule-default-months 3)
(setq my-random-schedule-exponent 1)

;; Run with caution. Make backups before first run.
```

`org-queue` initializes automatically at Emacs startup:
- Builds the two queues in memory (no daily disk cache).
- Prunes not‑due entries.
- Displays the top outstanding task.

---

## Key Concepts

### Two‑Queue Architecture
- **Outstanding** — items **available now** (respecting deferrals). Shown immediately by `org-queue-show-top`.  
- **Pending (today)** — items **available later today**. They are automatically promoted to Outstanding when their `available-at` time arrives (or next refresh).

**Availability calculation (non‑SRS):**  
`available-at = max(SCHEDULED, LAST_REPEAT + deferral(priority))`, with optional QFORCE override (see [Scheduling & Snooze](#scheduling--snooze)).

### Scheduling Invariants
Every schedule write runs through a single constrained path:
- **Weekdays only:** weekends are pushed to the next Monday.  
- **Work window:** snapped into `[09:00, 18:00)` (local time).  
- **Bounded jitter:** ±10 minutes (kept inside the window).  
- **Never in the past:** always `>= now`.  
- **Respect LAST_REPEAT deferral** for non‑SRS entries.

Commands using this path:
- `my-schedule-command`, `my-advance-schedule`, `my-postpone-schedule`, and the internals they call.

### SRS Integration
- An entry is SRS‑managed if it or its parent contains an `:SRSITEMS:` drawer (name is configurable).
- For SRS items, **next due** is read from the drawer (starred row preferred; otherwise the earliest timestamp).
- SRS items are **mixed** into the head of the Outstanding queue by a configurable ratio and start policy.
- Optional **conceal**: only show “Front,” hide “Back/Answer” bodies when visiting an SRS item.

### Night Shift
- During night shift (default **22:00–06:00**):
  - **SRS is suppressed** (kept out of Outstanding; still tracked as Pending).
  - `my-launch-anki` is disabled.
- You can change the window or disable night shift entirely.

### ID Guard
- **Never hangs on bad IDs**: resolution time‑boxed to 0.12s, with a 300s cooldown per failing ID.
- **Skips remote/TRAMP files** for resolution (fast fail).
- **Auto‑drop** unresolved entries during maintenance/midnight refresh or when visited.
- **Idle coalesced refresh**: after saving a file, re‑resolve only the tasks that belong to that file (no global scans).

---

## Keybindings (Global Prefix: `C-;`)

> The package installs one **global prefix map** bound to `C-;`.  
> Rebind if you prefer:  
> ```emacs-lisp
> (global-unset-key (kbd "C-;"))
> (global-set-key (kbd "C-c q") org-queue-prefix-map)  ;; example
> ```

| Keys         | Command                                  | Description |
|--------------|-------------------------------------------|-------------|
| `C-; ,`      | `my-set-priority-with-heuristics`         | Set a numeric priority within a heuristic range (0–9 → 1..64). |
| `C-; +`      | `my-increase-priority-range`              | Move to a **higher urgency** range (lower range number). |
| `C-; -`      | `my-decrease-priority-range`              | Move to a **lower urgency** range. |
| `C-; s`      | `my-schedule-command`                     | Prompt months, schedule with constraints, set/confirm priority. |
| `C-; a`      | `my-advance-schedule`                     | Advance using \(x - 1/\ln(x+e)\) adjusted by priority. |
| `C-; p`      | `my-postpone-schedule`                    | Postpone using \(x + 1/\ln(x+e)\) adjusted by priority. |
| `C-; c`      | `org-queue-stamp-and-show-top`            | Stamp `:LAST_REPEAT:` here (non‑SRS) and jump to the top outstanding task. |
| `C-; w`      | `widen-and-recenter`                      | Widen, reset folding, recenter. |
| `C-; u`      | `org-show-parent-heading-cleanly`         | Show parent context (narrow to subtree). |
| `C-; D`      | `org-demote-subtree`                      | Demote subtree. |
| `C-; P`      | `org-promote-subtree`                     | Promote subtree. |
| `C-; x`      | `org-interactive-extract`                 | Create an `{{extract#ID|…|ID}}` and child heading. |
| `C-; X`      | `org-remove-all-extract-blocks`           | Remove all `extract` blocks in current buffer. |
| `C-; A`      | `my-launch-anki`                          | Launch/focus Anki (disabled during night shift). |

**Conditional bindings (loaded when available):**

| Keys         | Requires        | Command                            | Description |
|--------------|-----------------|-------------------------------------|-------------|
| `C-; 1`      | `org-srs`       | `org-queue-srs-rate-again`          | Rate SRS entry “again”. |
| `C-; 3`      | `org-srs`       | `org-queue-srs-rate-good`           | Rate SRS entry “good”. |
| `C-; S`      | `org-srs`       | `org-queue-srs-item-create-card`    | Create an SRS card at point. |
| `C-; z`      | (none)          | `org-interactive-cloze`             | Create cloze (front shows prefix/[…]/suffix). |
| `C-; Z`      | (none)          | `org-interactive-cloze-prefix`      | Cloze, front shows **prefix only**. |
| `C-; M-z`    | (none)          | `org-interactive-cloze-suffix`      | Cloze, front shows **suffix only**. |
| `C-; l`      | `org-web-tools` | `org-web-tools-insert-link-for-url` | Insert URL link. |
| `C-; I`      | `org-web-tools` | `org-web-tools-insert-web-page-as-entry` | Capture page into Org. |

Other frequently used interactive functions (not bound by default):
- `org-queue-show-top` — show the current head of Outstanding.
- `my-reset-outstanding-tasks-index` — reset index and head.
- `my-reset-and-show-current-outstanding-task` — reset and show head (also launches Anki).

---

## Configuration

All options are in the `org-queue` group:  
`M-x customize-group RET org-queue`

### Essential
```emacs-lisp
(setq org-queue-directory "~/org")        ;; recursive .org discovery
(setq my-queue-visible-buffer-count 8)    ;; limit distinct queue buffers kept visible
```

### Scheduling & Snooze
```emacs-lisp
;; Default range used when scheduling without explicit months
(setq my-random-schedule-default-months 3)
(setq my-random-schedule-exponent 1)      ;; 0=uniform, 1=quadratic, 2=cubic (bias later)

;; Deferral for non‑SRS items when snoozed via LAST_REPEAT:
;; f(p) = BASE + SLOPE * priority (1..64). Higher priority => longer/shorter depending on slope.
(setq org-queue-non-srs-snooze-base-minutes 0)
(setq org-queue-non-srs-snooze-slope-minutes 1)

;; QFORCE semantics:
;; When non‑nil, QFORCE ignores LAST_REPEAT deferral and becomes immediately available.
(setq org-queue-qforce-ignores-last-repeat nil)
```

### SRS & Mixing
```emacs-lisp
;; Interleave blocks of non‑SRS and SRS in the head:
(setq org-queue-srs-mix-ratio '(1 . 4))    ;; (NON-SRS . SRS)

;; Who starts the head block: 'non-srs, 'srs, 'rotate, or 'auto
(setq org-queue-mix-start 'rotate)

;; Conceal “Back/Answer” body when visiting SRS entries
(setq org-queue-srs-conceal-answer t)
```

### Night Shift Window
```emacs-lisp
(setq org-queue-night-shift-enabled t)
(setq org-queue-night-shift-start "22:00")
(setq org-queue-night-shift-end   "06:00")
```

### Performance & Files
```emacs-lisp
;; Where org-queue stores its own cache files (e.g., maintenance stamp, org-id DB)
;; Defaults under ~/.emacs.d/var/org-queue/
(setq org-queue-cache-dir (expand-file-name "org-queue/" user-emacs-directory))

;; File roster cache TTL (seconds) before re-scanning org-queue-directory
(setq org-queue-file-cache-ttl 10)
```

**Notes:**
- The project **does not depend on `org-agenda-files`**. It recursively scans `org-queue-directory`.
- IDs are stored via Org’s `org-id`; this project places the ID database under `org-queue-cache-dir` by default.
- `large-file-warning-threshold` is disabled in this package (set to `nil`) and randomness is seeded.

---

## Maintenance Pipeline

Run on demand:
```emacs-lisp
M-x org-queue-maintenance RET
```

What it does (batched saves; UI suppressed; one final save):
1. (Optional) `org-roam-db-sync` if present.  
2. Ensure **priorities** and **schedules** exist for all relevant headings (skips DONE, skips SRS scheduling).  
3. **Advance** near‑future schedules (defaults to `2^8 = 256` entries).  
4. Auto‑postpone **overdue** TODOs.  
5. Postpone **duplicate priorities per file** (keep one outstanding per numeric priority).  
6. Enforce **monotone cap**: lower priorities can’t outnumber higher ones; overflow postponed FIFO.  
7. Re‑ensure priorities/schedules where needed.  
8. Postpone **consecutive same‑file** tasks (keep only the first in a row).  
9. **Clean DONE** tasks (remove `SCHEDULED` and `PRIORITY`).  
10. Rebuild queues (and run ID guard on them), show top, stamp the run date.

**Automatic gating:**  
- `org-queue-maintenance-on-startup` = `'if-needed` (default) runs once per day after startup idle.  
- Midnight: queues are rebuilt and the next midnight refresh is scheduled.

---

## Custom Syntax: Extracts & Clozes

Inline markers render cleanly via overlays (wrappers hidden; payload styled):

- **Extracts:** `{{extract#ID|PAYLOAD|ID}}`  
  - Create with `C-; x` → inserts marker and creates a child heading with cleaned text.
- **Cloze deletions:**  
  - Back: `{{clozed#ID|SECRET|ID}}`  
  - Front ellipsis: `{{cloze#ID|[...]|ID}}`  
  - Create with:
    - `C-; z` (prefix + […] + suffix),  
    - `C-; Z` (prefix only),  
    - `C-; M-z` (suffix only).
- Toggle marker visibility (wrappers) in current buffer: `M-x org-toggle-syntax-markers`.

---

## Optional Integrations

- **SRS** (`org-srs`): rating outside reviews (`C-; 1` / `C-; 3`), and card creation (`C-; S`).  
- **Anki orchestration:** `my-launch-anki` is cross‑platform and night‑shift aware.
- **gptel:** After `gptel-send`, `org-queue` tries to bring Anki to front (if not night shift).
- **org-web-tools:** Link/page capture bound under the prefix when detected.
- **org-roam:** One‑shot DB sync during maintenance (if available).

---

## Migration Notes (Breaking Changes)

This README reflects the **current** code (the earlier README you shared is outdated). Key deltas:

1. **Global prefix changed**  
   - **Was:** `C-c q`  
   - **Now:** **`C-;`** (you can rebind to `C-c q` if desired).

2. **Chooser UI removed**  
   - The prior tabulated “chooser with bulk ops” module is no longer present.
   - The system now relies on the two‑queue model with **live micro‑updates** and single‑head navigation (`org-queue-show-top`).

3. **No daily disk cache for the queue**  
   - The queues are built in memory on startup and refreshed incrementally; only a small **maintenance stamp** and Org ID DB are persisted.

4. **Scheduling invariants are mandatory**  
   - Weekday‑only, snapped to work window with bounded jitter; **all** scheduling flows go through one constrained path.

5. **SRS interleave & night shift are first‑class**  
   - Configurable **mix ratio**, **start policy**, and concealment; SRS suppressed during night shift by default.

6. **ID Guard introduced**  
   - Time‑boxed ID resolution, cooldown on failure, TRAMP skip, auto‑drop of unrecoverable entries, idle per‑file refresh on save.

If you had custom keybindings or references to old chooser functions, remove them and bind the new prefix map or functions you need.

---

## Troubleshooting

- **“Nothing shows up at the top”**  
  - Ensure `org-queue-directory` points to the correct root and contains `.org` files.  
  - Night shift may be active; SRS items are intentionally suppressed.  
  - Item might not yet be due; run `org-queue-show-top` to prune head and refresh display.

- **“My SRS items don’t appear”**  
  - Check that entries use the configured SRS drawer name (defaults to `SRSITEMS`).  
  - During night shift, SRS won’t surface in Outstanding (by design).  
  - Verify due timestamps: only **today** (or overdue) timestamps are in scope.

- **“QFORCE doesn’t pull it in immediately”**  
  - If `org-queue-qforce-ignores-last-repeat` is **nil**, QFORCE still respects `LAST_REPEAT + deferral`.  
  - Set it to `t` to make QFORCE entries immediately available.

- **“IDs got messed up”**  
  - ID guard will drop unrecoverable items automatically; visit the entry or re‑create its ID (`org-id-get-create`) if needed.  
  - Remote/TRAMP files are skipped for resolution.

- **“Scheduling landed on a weekday morning with a slight offset”**  
  - That’s intentional: weekday‑only, work‑window snapping, and small jitter.

---

## License

GPL-3.0 — see [`LICENSE`](./LICENSE).

---

### Notes for Contributors

- Follow the single‑path scheduling and single‑path ID resolution design.  
- Keep live micro‑update semantics (advice + `after-change-functions`) intact.  
- Avoid introducing global caches for queues; favor in‑memory state + batched saves.  
