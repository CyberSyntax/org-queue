# org-queue

**org-queue** is an Emacs package designed for incremental task management within **org-mode**, bringing advanced priority handling, scheduling, and queuing capabilities to help you stay organized and focused. Inspired by incremental learning techniques, org-queue helps you prioritize, defer, and sequentially handle tasks according to urgency and importance.

## Features

- **Incremental Task Prioritization**: Tasks are organized using a **priority range of 1 to 64**, providing highly granular control over importance.
- **Dynamic Scheduling**: Tasks are automatically scheduled within a specified time frame to distribute workload evenly.
- **Randomized Prioritization**: Priorities are randomly assigned within configurable ranges to avoid bias.
- **Outstanding Task Management**: Easily track overdue tasks and navigate to tasks due today or in the near future.
- **Queue-Based Task Navigation**: Move seamlessly through a prioritized task queue, focusing on one task at a time.

## Why org-queue?

org-queue leverages the powerful capabilities of **org-mode** to provide a dynamic task management system. It combines structured queuing, randomization, and progressive scheduling to help you efficiently tackle tasks while maintaining flexibility.

## Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/CyberSyntax/org-queue.git
    ```

2. Add `org-queue` to your Emacs configuration:
    ```emacs-lisp
    ;; Set the path to the org-queue directory
    (setq org-queue-dir "/path/to/org-queue/")
    (add-to-list 'load-path (file-name-as-directory org-queue-dir))

    ;; Load org-queue
    (require 'org-queue)
    ```

Replace `"/path/to/org-queue/"` with the directory where you cloned the repository. For example:  
`/home/username/emacs/org-queue/`

3. Restart Emacs, and you're ready to use `org-queue`!

## Usage

org-queue enhances your **org-mode** workflow with powerful commands for task prioritization and navigation:

### Task Prioritization

- Priorities are managed automatically within the range you configure:
    - Highest priority: **1**
    - Lowest priority: **64**
    - Default priority: **32**

- Random priorities are assigned when creating new tasks, ensuring balanced task distribution.

### Task Scheduling

Tasks are scheduled randomly within the next **two months**, helping you distribute tasks evenly over time.

### Queue Navigation Commands

- **`C-c q n`**: Navigate to the **next outstanding task**.  
- **`C-c q p`**: Navigate to the **previous outstanding task**.  
- **`C-c q c`**: Show the **current outstanding task**.  
- **`C-c q r`**: Reset the queue to the **first outstanding task**.

With these commands, you can seamlessly move through your task queue without manually searching through your Org files.

### Configuration

If needed, you can customize the priority range in your `init.el`:

```emacs-lisp
(setq org-priority-highest 1)   ;; Highest priority
(setq org-priority-lowest 64)   ;; Lowest priority
(setq org-priority-default 32)  ;; Default priority
```

This setup allows org-queue to work perfectly with your tasks and priorities.

## Workflow Example

1. **Insert Tasks**: Use `C-<return>` to create a new heading in org-mode.  
   - Tasks will automatically be assigned a **random priority** and **random schedule**.

2. **Navigate Tasks**:  
   - Use `C-c q n` and `C-c q p` to move through your outstanding tasks.  

3. **Focus on What Matters**:  
   - Priorities and scheduling adapt dynamically, keeping your workload balanced.

## License

This project is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
See the [LICENSE](./LICENSE) file for details.
