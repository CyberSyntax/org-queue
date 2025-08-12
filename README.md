# Org-queue

⚠️ **WARNING: USE WITH CAUTION** ⚠️  
org-queue is a powerful package that can irreversibly alter your org files.  

Once activated, it modifies task structures and schedules in ways that may not be easily reversible. If you’re not fully familiar with how the code works or what it’s doing under the hood, do not use it yet.  

Read the source. Understand the logic. Backup your files.  
You’ve been warned.  

---

**Note**: This README reflects the current modular architecture and features as of the latest code version.

# Table of Contents

1. [Org-queue](#org-queue)  
   1. [Key Features](#key-features)  
      1. [Granular Task Prioritization](#granular-task-prioritization)  
      2. [Dynamic and Randomized Scheduling](#dynamic-and-randomized-scheduling)  
      3. [Outstanding Task Tracking](#outstanding-task-tracking)  
      4. [Queue-Based Navigation with Intelligent SRS Integration](#queue-based-navigation-with-intelligent-srs-integration)  
      5. [Comprehensive Task Processing System](#comprehensive-task-processing-system)  
      6. [Advanced Display and UI Features](#advanced-display-and-ui-features)  
   2. [Why Choose org-queue?](#why-choose-org-queue)  
   3. [Installation](#installation)  
      1. [Clone the Repository](#clone-the-repository)  
      2. [Configure org-queue in Emacs](#configure-org-queue-in-emacs)  
      3. [Restart Emacs](#restart-emacs)  
   4. [Usage](#usage)  
      1. [Key Commands and Shortcuts](#key-commands-and-shortcuts)  
   5. [Modular Architecture](#modular-architecture)  
   6. [Configuration](#configuration)  
      1. [Customize the Default Scheduling Range](#customize-the-default-scheduling-range)  
      2. [Adjust the Scheduling Bias](#adjust-the-scheduling-bias)  
      3. [SRS Integration Settings](#srs-integration-settings)  
      4. [Customize Priority Ranges](#customize-priority-ranges)  
      5. [Important Notes](#important-notes)  
      6. [Sample Configuration](#sample-configuration)  
   7. [License](#license)  

# Org-queue

**org-queue** is a comprehensive Emacs package that transforms **org-mode** into a sophisticated incremental task management system. Built with a modular architecture, it provides intelligent task prioritization, dynamic scheduling, queue-based navigation, and seamless integration with spaced repetition systems (SRS). Drawing inspiration from SuperMemo and incremental learning methodologies, org-queue enables efficient task processing with advanced content extraction and cloze deletion capabilities.

# Key Features

## Granular Task Prioritization

Assign priorities on a scale of **1 (highest)** to **64 (lowest)** for precise task ranking. Features heuristic-based priority assignment and spreading priorities across task selections.

## Dynamic and Randomized Scheduling

Automatically distribute tasks across a period using mathematical distributions. Supports randomized scheduling with configurable bias and priority-based adjustments. Includes functions for advancing/postponing schedules with diminishing returns.

## Outstanding Task Tracking

Efficiently monitor and process overdue tasks. Maintains a persistent queue of outstanding tasks with automatic rebuilding and deduplication. Supports forcing tasks as outstanding regardless of schedule.

## Queue-Based Navigation with Intelligent SRS Integration

Work sequentially through a prioritized task queue. Seamless SRS session management with exhaustion detection. Platform-adaptive Anki launching (desktop/Android). Configurable reviews per task.

## Comprehensive Task Processing System

Automatic startup processing including:
- Overdue task postponement
- Priority constraint enforcement
- Duplicate task management
- DONE task cleanup
- Consecutive same-file task postponement

Full persistence of task queue and index across sessions.

## Advanced Display and UI Features

- Custom syntax highlighting for extracts/clozes
- Interactive content creation (extracts, clozes)
- Tabulated queue chooser with marking and bulk operations
- Pulse highlighting and view control
- Flag-based status display
- Optional web/AI integrations

# Why Choose org-queue?

org-queue transforms org-mode into a full incremental learning and task management system. With modular design, it offers advanced content processing, intelligent prioritization, SRS integration, and sophisticated scheduling while maintaining compatibility with standard org-mode workflows. Inspired by SuperMemo, it brings incremental learning to Emacs.

# Installation

## Clone the Repository

Open your terminal and run:  
```sh
git clone https://github.com/CyberSyntax/org-queue.git
```

## Configure org-queue in Emacs

Add to your Emacs configuration file (`.emacs` or `init.el`):

```emacs-lisp
;; Directory Setup
(setq user-emacs-directory "path/to/emacs/dir")  ;; For cache-dir
(setq org-agenda-directory "path/to/org-agenda")
(setq org-agenda-files (directory-files-recursively org-agenda-directory "\\.org$"))
(setq org-queue-directory org-agenda-directory)

;; Optional: Load org-srs (if using)
(when (file-exists-p "path/to/org-srs")
  (add-to-list 'load-path "path/to/org-srs")
  (require 'org-srs))

;; Load org-queue
(add-to-list 'load-path "path/to/org-queue")
(require 'org-queue)

;; Configuration Examples
(setq my-random-schedule-default-months 3)
(setq org-queue-preinit-srs nil)
```

## Restart Emacs

org-queue is now ready.

# Usage

org-queue integrates with org-mode for incremental task management.

## Key Commands and Shortcuts

Global prefix: `C-;`

- Navigation:  
  `f` Next task  
  `b` Previous task  
  `c` Current task  
  `B` Open queue chooser  
  `C` Open subset chooser from buffer  
  `r` Remove current  
  `R` Reset and show first  
  `m` Move to position  

- Priority:  
  `,` Set with heuristics  
  `i` Increase range  
  `d` Decrease range  

- Scheduling:  
  `s` Schedule with priority  
  `a` Advance  
  `p` Postpone  

- View:  
  `w` Widen/recenter  
  `n` Narrow subtree  
  `u` Show parent  

- Editing:  
  `D` Demote subtree  
  `P` Promote subtree  
  `W` Cut subtree  
  `Y` Paste subtree  

- Content:  
  `x` Create extract  
  `X` Remove extracts  
  `z` Create cloze (if org-srs)  

- SRS (if available):  
  `1` Rate again  
  `3` Rate good  

- Optional:  
  `l` Insert URL link (if org-web-tools)  
  `I` Insert web page (if org-web-tools)  
  `g` GPT chat (if gptel)  

Automatic processes run at startup for task maintenance.

# Modular Architecture

- `org-queue.el`: Entry point, keybindings  
- `org-queue-config.el`: Settings, priorities  
- `org-queue-tasks.el`: Queue management  
- `org-queue-priority.el`: Priority logic  
- `org-queue-schedule.el`: Scheduling algorithms  
- `org-queue-display.el`: UI, highlighting, chooser  
- `org-queue-utils.el`: Utilities  
- `org-queue-srs-bridge.el`: SRS integration  

# Configuration

Customize via `M-x customize-group RET org-queue` or init file.

## Customize the Default Scheduling Range

```emacs-lisp
(setq my-random-schedule-default-months 3)
```

## Adjust the Scheduling Bias

```emacs-lisp
(setq my-random-schedule-exponent 1)  ;; 0: uniform, 1: quadratic, 2: cubic
```

## SRS Integration Settings

```emacs-lisp
(setq org-queue-preinit-srs t)  ;; Pre-init SRS
```

## Customize Priority Ranges

```emacs-lisp
(setq my-priority-ranges
      '((0 . (1 . 2))
        (1 . (2 . 5))
        ;; ... up to 9
        (9 . (58 . 64))))
```

## Important Notes

- Load org-queue before settings  
- Use `setq` for config  
- Backup org files  

## Sample Configuration

```emacs-lisp
(add-to-list 'load-path "path/to/org-queue")
(require 'org-queue)
(setq my-random-schedule-default-months 6)
(setq my-random-schedule-exponent 2)
(setq org-queue-preinit-srs t)
```

# License

GNU General Public License v3.0. See [LICENSE](./LICENSE).
