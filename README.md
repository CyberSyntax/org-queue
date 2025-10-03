# Org-queue

⚠️ **WARNING: USE WITH CAUTION** ⚠️  
org-queue is a powerful package that can irreversibly alter your org files.  

Once activated, it modifies task structures and schedules in ways that may not be easily reversible. If you're not fully familiar with how the code works or what it's doing under the hood, do not use it yet.  

Read the source. Understand the logic. Backup your files.  
You've been warned.  

---

**Note**: This README reflects the current modular architecture and features as of the latest code version.

# Table of Contents

1. [Overview](#overview)
2. [Key Features](#key-features)
   - [Chooser UI with Bulk Operations](#chooser-ui-with-bulk-operations)
   - [Granular Task Prioritization](#granular-task-prioritization)
   - [Dynamic and Randomized Scheduling](#dynamic-and-randomized-scheduling)
   - [Outstanding Task Tracking](#outstanding-task-tracking)
   - [Queue-Based Navigation](#queue-based-navigation)
   - [Intelligent SRS Integration](#intelligent-srs-integration)
   - [Comprehensive Task Processing](#comprehensive-task-processing)
   - [Advanced Display Features](#advanced-display-features)
3. [Why Choose org-queue?](#why-choose-org-queue)
4. [Installation](#installation)
5. [Usage](#usage)
   - [Global Commands](#global-commands)
   - [Chooser Commands](#chooser-commands)
   - [Queue Switching](#queue-switching)
6. [Modular Architecture](#modular-architecture)
7. [Configuration](#configuration)
8. [Workflows](#workflows)
   - [SuperMemo-Style Add to Outstanding](#supermemo-style-add-to-outstanding)
   - [Incremental Reading with Extracts](#incremental-reading-with-extracts)
   - [Bulk Task Management](#bulk-task-management)
9. [License](#license)

# Overview

**org-queue** is a comprehensive Emacs package that transforms **org-mode** into a sophisticated incremental task management system. Built with a modular architecture, it provides:

- **Tabulated chooser UI** with marking and bulk operations
- **Intelligent task prioritization** (1-64 scale with heuristics)
- **Dynamic scheduling** with priority-based bias
- **Queue-based navigation** with persistence
- **Seamless SRS integration** (org-srs compatible)
- **Content extraction** and cloze deletion
- **Buffer visibility management** for focused workflows
- **ID-based task tracking** with automatic deduplication

Drawing inspiration from SuperMemo and incremental learning methodologies, org-queue enables efficient task processing with advanced content manipulation capabilities.

# Key Features

## Chooser UI with Bulk Operations

**Interactive tabulated list interface** for queue management:
- **Browse tasks** in a fixed-width, sortable table
- **Mark tasks** (Dired-style) for bulk operations
- **Region marking** for quick selection
- **Subset mode** to filter tasks from current buffer/region
- **Spread scheduling** across selections with configurable density
- **Spread priorities** evenly over marked tasks
- **Bulk advance/postpone** with mathematical distributions
- **Add to outstanding** (SuperMemo-style QFORCE)
- **Reorder queue** with move up/down/to-position
- **Tab integration** for multi-tasking
- **Row-wise navigation** (j/k keys) that skips wrapped lines
- **ASCII sanitization** for stable column alignment

Access:
- `C-c q B` - Open chooser for global queue
- `C-c q C` - Open chooser from current buffer (subset mode)
- Within chooser: `?` for help

## Granular Task Prioritization

**Numeric priority system** (1=highest, 64=lowest):
- **Heuristic assignment** with 10 configurable ranges (0-9)
- **Priority spreading** across selections with linear interpolation
- **Range controls** for quick adjustments (i/d to increase/decrease)
- **Dual storage**: PRIORITY property + [#N] cookie
- **Flag grouping**: Visual categorization (Flag:1 through Flag:7)
- **Flag-based status** display showing position within flag group

## Dynamic and Randomized Scheduling

**Mathematical scheduling distributions**:
- **Priority-biased** power-law distribution (configurable exponent)
- **Randomized** scheduling within bounds (default: 3 months)
- **Advance schedule**: Diminishing returns via \( f(x) = x - \frac{1}{\ln(x + e)} \)
- **Postpone schedule**: Increasing delays via \( f(x) = x + \frac{1}{\ln(x + e)} \)
- **Spread scheduling**: Distribute selection across period (items/day configurable)
- **Priority enhancement**: High priority tasks get earlier dates
- **Overdue handling**: Smart minimum times (today for overdue, tomorrow for future)
- **SRS-aware**: Skips scheduling for entries with org-srs drawers

## Outstanding Task Tracking

**Persistent queue management**:
- **Outstanding detection**: Scheduled today/earlier OR QFORCE property set
- **Deduplication**: By ID (or file@position fallback)
- **Automatic rebuilding** with index persistence
- **Force outstanding**: SuperMemo-style "Add to outstanding" (sets QFORCE=1)
- **Cache system**: Daily task list persisted as (file . id) pairs
- **Index synchronization**: Device-specific index file
- **Buffer visibility limiting**: Keep only N distinct buffers visible (default: 8)
- **Two-tier sorting**: TODO status first, then priority
- **Marker resolution**: Lazy ID lookup with targeted refresh

## Queue-Based Navigation

**Efficient task traversal**:
- **Sequential navigation**: Next/previous with wraparound
- **Current task** display with pulse highlighting
- **Queue switching**: Completion-based interface showing all tasks
- **Tab integration**: Open tasks in new tabs (foreground/background)
- **Remove task**: Takes you to next in queue
- **Move to position**: Jump to specific queue index
- **Reset index**: Return to first task
- **Buffer limiting**: Automatic tidying of non-active queue buffers

## Intelligent SRS Integration

**Seamless org-srs workflow**:
- **Session management**: Auto-start/stop review sessions
- **Review counting**: Configurable reviews-per-task (default: 4)
- **Exhaustion detection**: Auto-return to queue when done
- **Anki orchestration**: Launch/focus Anki before reviews
- **Platform-adaptive**: Desktop (wmctrl/xdotool) and Android support
- **Entry detection**: Identifies SRS entries (current/parent)
- **Rating commands**: Direct entry rating outside reviews (again/good)
- **Card creation**: One-command SRS card setup
- **Skip logic**: Avoids scheduling SRS-managed entries

## Comprehensive Task Processing

**Automated maintenance**:
- **Quick maintenance** (fast):
  - Postpone overdue tasks
  - Remove duplicate priorities per file
- **Full maintenance** (comprehensive):
  - Ensure all headings have priorities/schedules
  - Advance near-future schedules (2^8 = 256 tasks)
  - Postpone overdue tasks
  - Enforce priority constraints (monotone cap)
  - Remove consecutive same-file duplicates
  - Clean DONE tasks (remove SCHEDULED/PRIORITY)
- **Startup processing**: Automatic on Emacs launch
- **Index persistence**: Saves position across sessions
- **Deduplication**: ID-based with file@position fallback

## Advanced Display Features

**Custom syntax and highlighting**:
- **Extract blocks**: `{{extract:...}}` for incremental reading
- **Cloze deletions**: `{{clozed:...}}` for active recall
- **Syntax highlighting**: Color-coded overlays for extracts/clozes
- **Interactive creation**:
  - `org-interactive-extract`: Select text → create extract + child heading
  - `org-interactive-cloze`: Select text → create cloze + child heading with [...] placeholder
- **Bulk removal**: `org-remove-all-extract-blocks` after processing
- **Pulse highlighting**: Temporary visual feedback on current line
- **View control**: Widen/narrow/recenter/show-parent commands
- **Flag status**: Shows flag group, position, and remaining count
- **Marker visibility**: Toggle display of syntax markers

# Why Choose org-queue?

org-queue transforms org-mode into a **full incremental learning system**:

1. **Modular design**: Clean separation of concerns, easy to customize
2. **Advanced content processing**: Extracts and clozes with parent-child priority inheritance
3. **Intelligent prioritization**: 64-level granularity with heuristic ranges
4. **Sophisticated scheduling**: Priority-biased power-law distributions
5. **SRS integration**: Seamless org-srs + Anki workflow
6. **Chooser UI**: Dired-like bulk operations for efficient task management
7. **Buffer management**: Automatic visibility limiting for focused work
8. **ID-based tracking**: Reliable deduplication and marker resolution
9. **Persistence**: Queue and index survive restarts
10. **org-mode compatible**: Standard workflows still work

Inspired by SuperMemo, it brings **incremental learning to Emacs** with a modern, modular foundation.

# Installation

## Prerequisites

- Emacs 27.1+ (with `tabulated-list-mode`)
- org-mode 9.0+
- Optional: org-srs (for SRS features)
- Optional: org-web-tools (for web capture)
- Optional: gptel (for AI integration)

## Clone the Repository

```sh
git clone https://github.com/CyberSyntax/org-queue.git
```

## Configure org-queue in Emacs

Add to your Emacs configuration file (`.emacs` or `init.el`):

```emacs-lisp
;; Directory Setup
(setq user-emacs-directory "~/.emacs.d")  ;; For cache-dir
(setq org-queue-directory "~/org")        ;; Your org files root

;; Optional: Load org-srs (if using)
(when (file-exists-p "~/path/to/org-srs")
  (add-to-list 'load-path "~/path/to/org-srs")
  (require 'org-srs))

;; Load org-queue
(add-to-list 'load-path "~/path/to/org-queue")
(require 'org-queue)

;; Configuration Examples (optional)
(setq my-random-schedule-default-months 3)
(setq my-random-schedule-exponent 1)
(setq org-queue-preinit-srs nil)
(setq my-queue-visible-buffer-count 8)  ;; Number of queue buffers to keep visible
```

## Restart Emacs

org-queue will auto-initialize on startup.

# Usage

org-queue provides three main interaction modes:

1. **Global commands** (via `C-c q` prefix)
2. **Chooser UI** (tabulated list)
3. **Queue switching** (completion interface)

## Global Commands

**Prefix**: `C-c q` (configurable)

### Navigation
- `f` - Next outstanding task
- `b` - Previous outstanding task
- `c` - Show current outstanding task
- `F` - Next task in new tab
- `B` - Previous task in new tab
- `C` - Current task in new tab
- `r` - Remove current task from queue
- `R` - Reset index and show first task
- `m` - Move current task to position (1-based)

### Chooser
- `Q` - Open chooser for global queue
- `q` - Open chooser from current buffer (subset mode)

### Priority
- `,` - Set priority with heuristics (interactive range selection)
- `i` - Increase priority range (lower number = higher priority)
- `d` - Decrease priority range (higher number = lower priority)

### Scheduling
- `s` - Schedule task with priority bias
- `a` - Advance schedule (diminishing returns)
- `p` - Postpone schedule (increasing delays)

### View Control
- `w` - Widen buffer and recenter
- `u` - Show parent heading context

### Structure Editing
- `D` - Demote subtree
- `P` - Promote subtree

### Content Creation
- `x` - Create extract block (SuperMemo-style)
- `X` - Remove all extract blocks (after processing)
- `z` - Create cloze deletion (if org-srs available)
- `Z` - Create SRS card in current entry (if org-srs available)

### SRS (if org-srs available)
- `1` - Rate entry as "again" (difficult)
- `3` - Rate entry as "good" (easy)
- `A` - Launch/focus Anki

### Optional Integrations
- `l` - Insert URL link (if org-web-tools installed)
- `I` - Insert web page as entry (if org-web-tools installed)
- `g` - Start GPT chat (if gptel installed)

## Chooser Commands

Access with `C-c q Q` (global queue) or `C-c q q` (subset from buffer)

### Navigation (Row-wise)
- `j` - Next row (skips wrapped lines)
- `k` - Previous row (skips wrapped lines)

### Visit/Open
- `RET` / `f` - Visit task (updates global index if in global mode)
- `o` - Visit task in other window (keep chooser selected)
- `T` - Visit in new tab (background)
- `C-c t` - Visit in new tab (foreground)

### Marking (Dired-style)
- `m` - Mark current row
- `u` - Unmark current row
- `t` - Toggle mark on current row
- `U` - Unmark all
- `M` - Mark region (all rows in active region)

### Bulk Operations (on marked/region/current)
- `S` - Spread schedule across period (prompts: start date, days, items/day)
- `H` - Spread priorities (prompts: low/high priority bounds)
- `a` - Advance marked tasks
- `p` - Postpone marked tasks
- `O` - Add to outstanding (sets QFORCE, nudges priority toward 1)
  - With prefix (`C-u O`): Remove QFORCE

### Reordering (Global Queue Only)
- `M-p` / `M-<up>` - Move current task up
- `M-n` / `M-<down>` - Move current task down
- `M-g M-g` - Move to position (prompts for 1-based position)

### Refresh
- `g` - Refresh chooser view (soft)
- `G` - Hard refresh (reindex files, rebuild queue, update chooser)
- `C-c T` - Open chooser in dedicated tab-bar tab

### Subset Mode (from `C-c q q`)
- `C` - Create new subset from current buffer/region
- **Note**: Subset mode disables reordering (global queue only)

### Other
- `q` - Quit chooser window
- `n` - Disabled (prevents conflict with next-line)

### Selection Logic for Bulk Operations
1. If any marks exist → use marked rows
2. Else if region is active → use rows in region
3. Else → use current row

## Queue Switching

**Command**: `my-queue-switch-to-task`

- Not bound by default (recommend `C-c q s`)
- Completion interface showing all tasks: `"#### Title — file"`
- Preserves queue order in candidates
- Updates index and displays selected task

# Modular Architecture

org-queue is split into focused modules:

| Module | Purpose |
|--------|---------|
| `org-queue.el` | Entry point, global keymap, requires all modules |
| `org-queue-config.el` | Configuration, priority ranges, system detection |
| `org-queue-tasks.el` | Queue management, navigation, persistence, maintenance |
| `org-queue-priority.el` | Priority logic, heuristics, spreading, range controls |
| `org-queue-schedule.el` | Scheduling algorithms, advance/postpone, auto-operations |
| `org-queue-display.el` | UI, syntax highlighting, extracts/clozes, flag status |
| `org-queue-chooser.el` | Tabulated list UI, marking, bulk operations, subset mode |
| `org-queue-utils.el` | File indexing, map-entries, TODO/DONE detection |
| `org-queue-srs-bridge.el` | org-srs integration, session management, Anki launching |
| `org-queue-gptel-bridge.el` | GPTel integration (auto-launch Anki on gptel-send) |

# Configuration

Customize via `M-x customize-group RET org-queue` or in your init file.

## Essential Settings

```emacs-lisp
;; Directory where org files live (searched recursively)
(setq org-queue-directory "~/org")

;; Default scheduling range in months
(setq my-random-schedule-default-months 3)

;; Scheduling bias (0=uniform, 1=quadratic, 2=cubic)
(setq my-random-schedule-exponent 1)

;; Number of distinct queue buffers to keep visible
(setq my-queue-visible-buffer-count 8)
```

## Priority Ranges

10 ranges (0-9) mapping to priority groups:

```emacs-lisp
(setq my-priority-ranges
      '((0 . (1 . 2))    ; Flag:1
        (1 . (2 . 5))    ; Flag:2
        (2 . (5 . 12))   ; Flag:3
        (3 . (12 . 18))  ; Flag:4
        (4 . (18 . 24))  ; Flag:5
        (5 . (24 . 30))  ; Flag:6
        (6 . (30 . 37))  ; Flag:7
        (7 . (37 . 45))  ; Flag:7
        (8 . (45 . 58))  ; Flag:7
        (9 . (58 . 64)))) ; Flag:7
```

## SRS Integration

```emacs-lisp
;; Pre-initialize org-srs at startup (if installed)
(setq org-queue-preinit-srs t)

;; Number of SRS reviews before returning to queue
(setq my-srs-reviews-per-task 4)
```

## Chooser UI

```emacs-lisp
;; Column widths (must be integers)
(setq org-queue-chooser-mark-width 1)
(setq org-queue-chooser-index-width 5)
(setq org-queue-chooser-priority-width 4)
(setq org-queue-chooser-title-width 40)
(setq org-queue-chooser-preview-width 40)
(setq org-queue-chooser-sched-width 10)
(setq org-queue-chooser-file-width 28)

;; Open chooser in dedicated tab-bar tab
(setq org-queue-chooser-open-in-tab t)
```

## Persistence

```emacs-lisp
;; Cache file for task list (auto-set to cache-dir)
(setq my-outstanding-tasks-cache-file
      (expand-file-name "org-queue-outstanding-tasks.cache" cache-dir))

;; Index file for current position (device-specific)
(setq my-outstanding-tasks-index-file
      (expand-file-name "org-queue-index.cache" cache-dir))
```

## Important Notes

- **Load order**: `(require 'org-queue)` before customizations
- **Backup**: Always backup org files before first use
- **Cache-dir**: `user-emacs-directory` must be set for cache to work
- **org-srs**: Optional but recommended for full SRS workflow

# Workflows

## SuperMemo-Style Add to Outstanding

Force a future-scheduled task to appear in today's queue:

1. Open chooser: `C-c q Q`
2. Mark tasks: `m` (or `M` for region)
3. Add to outstanding: `O`
   - Sets `QFORCE=1` property
   - Nudges priority toward 1 (higher urgency)
4. Queue rebuilds automatically
5. Tasks now appear regardless of schedule

**Remove QFORCE**: `C-u O` on marked tasks

## Incremental Reading with Extracts

Process long documents incrementally:

1. Navigate to a heading in an org file
2. Select interesting text
3. Create extract: `C-c q x`
   - Wraps text in `{{extract:...}}`
   - Creates child heading with cleaned text
   - Inherits parent priority range
   - Assigns child a priority within that range
4. Repeat for multiple extracts
5. When done extracting: `C-c q X` (removes all extract blocks from parent)

**Result**: Parent becomes an index, children are schedulable micro-tasks.

## Bulk Task Management

Efficiently manage many tasks at once:

1. Open chooser: `C-c q Q`
2. Mark tasks:
   - `m` on individual rows
   - `M` to mark entire region
3. Spread schedule:
   - `S` → prompts: start date, days, items/day
   - Example: 30 tasks, 30 days, 1/day → evenly distributed
4. Spread priorities:
   - `H` → prompts: low/high bounds
   - Example: 10 tasks, 5-15 → priorities 5,6,7,8,9,10,11,12,13,14,15
5. Advance/postpone:
   - `a` for advance (all marked)
   - `p` for postpone (all marked)
6. Refresh: `g`

**Subset mode**: `C-c q q` to filter tasks from current buffer/region, then bulk-operate.

## Maintenance Workflows

**Quick maintenance** (daily):
```emacs-lisp
M-x org-queue-maintenance RET
```
- Postpone overdue
- Remove duplicate priorities

**Full maintenance** (weekly):
```emacs-lisp
C-u M-x org-queue-maintenance RET
```
- All quick tasks
- Ensure all headings have priority/schedule
- Advance near-future tasks
- Enforce priority constraints
- Clean DONE tasks

# License

GNU General Public License v3.0. See [LICENSE](./LICENSE).