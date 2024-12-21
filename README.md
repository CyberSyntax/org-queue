# org-queue

**org-queue** is an Emacs package designed to enhance **org-mode** with advanced incremental task management capabilities. By introducing features such as priority-based task handling, dynamic scheduling, and queue navigation, org-queue helps you stay focused and organized. Drawing inspiration from incremental learning techniques, it allows you to prioritize, defer, and manage tasks based on their urgency and importance.

## Key Features

- **Granular Task Prioritization**:  
  Assign priorities on a scale of **1 (highest)** to **64 (lowest)** for precise task ranking.

- **Dynamic and Randomized Scheduling**:  
  Automatically distribute tasks across a specified time frame, ensuring even workload distribution. Randomized prioritization minimizes bias when assigning task importance.

- **Outstanding Task Tracking**:  
  Effortlessly monitor overdue tasks and navigate through tasks due today or in the near future.

- **Queue-Based Navigation**:  
  Work sequentially through a prioritized task queue, maintaining focus on one task at a time.

## Why Choose org-queue?

org-queue builds upon the powerful foundation of **org-mode** to provide a robust and flexible task management system. By combining features like structured queues, randomization, and progressive scheduling, it enables users to manage tasks efficiently while maintaining flexibility and control.

---

## Installation

1. **Clone the Repository**:  
   Open your terminal and run:  
   ```sh
   git clone https://github.com/CyberSyntax/org-queue.git
   ```

2. **Configure org-queue in Emacs**:  
   Add the following lines to your Emacs configuration file (`.emacs` or `init.el`):  
   ```emacs-lisp
   ;; Set the path to the org-queue directory
   (setq org-queue-dir "/path/to/org-queue/")
   (add-to-list 'load-path (file-name-as-directory org-queue-dir))

   ;; Load org-queue
   (require 'org-queue)
   ```  
   Replace `/path/to/org-queue/` with the directory where you cloned the repository (e.g., `/home/username/emacs/org-queue/`).

3. **Restart Emacs**:  
   Once configured, restart Emacs, and org-queue will be ready to use!

---

## Usage

org-queue enhances your **org-mode** workflow by introducing powerful tools for task prioritization, scheduling, and queue navigation.

### Task Prioritization

Tasks in org-queue are assigned priorities based on the following scale:  
- **Highest Priority**: `1`  
- **Lowest Priority**: `64`  
- **Default Priority**: `32`  

When you create a new task, org-queue may randomly assign a priority (within configurable limits), ensuring a balanced workload distribution.

---

### Scheduling Tasks

**1. Non-Interactive Scheduling**:  
Create a new heading (e.g., with `C-<return>`). org-queue will automatically:  
- Assign a random priority.  
- Schedule the task randomly within the next **two months** (configurable).  

This ensures tasks are evenly distributed over time.

**2. Interactive Scheduling** (`C-c q s`):  
Press `C-c q s` to schedule a task interactively:  
- You’ll be prompted to specify the number of months ahead for scheduling.  
- The task will then be scheduled randomly within the selected range.  

For programmatic scheduling without user interaction, you can call `(my-random-schedule <months>)` directly.

---

### Navigating the Task Queue

org-queue provides several convenient commands for navigating outstanding tasks:

- **`C-c q n`**: Move to the next outstanding task.  
- **`C-c q p`**: Move to the previous outstanding task.  
- **`C-c q c`**: Show the current outstanding task.  
- **`C-c q r`**: Reset the queue to the first outstanding task.

These commands help you work sequentially through your task queue without needing to manually search your Org file.

---

## Configuration

To optimize your experience, configure org-mode’s priority settings to align with org-queue:

```emacs-lisp
(setq org-priority-highest 1)   ;; Highest priority
(setq org-priority-lowest 64)   ;; Lowest priority
(setq org-priority-default 32)  ;; Default priority
```

You can also adjust the default scheduling range by updating the `my-random-schedule-default-months` variable:

```emacs-lisp
(defvar my-random-schedule-default-months 3
  "Default number of months to schedule if none is specified.")
```

---

## Workflow Example

1. **Insert Tasks**:  
   Create a new heading (e.g., using `C-<return>`). org-queue will automatically assign a random priority and schedule the task within your configured months range.

2. **Interactive Scheduling**:  
   Use `C-c q s` to manually specify the scheduling range for any task.

3. **Navigate the Queue**:  
   Use commands like `C-c q n` and `C-c q p` to navigate outstanding tasks and stay focused.

This workflow balances automation (non-interactive scheduling) with manual intervention (interactive scheduling), offering a flexible and efficient approach to task management.

---

## License

org-queue is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
Refer to the LICENSE file for detailed terms and conditions.
