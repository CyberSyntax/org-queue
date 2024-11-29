# org-queue

**org-queue** is an Emacs package designed for incremental task management within **org-mode**, bringing advanced priority handling, scheduling, and queuing capabilities to help you stay organized and focused. Inspired by incremental learning techniques, org-queue helps you prioritize, defer, and sequentially handle tasks according to urgency and importance.

## Features

- **Incremental Task Prioritization**: Organize tasks by priority levels ranging from 1 to 64, accommodating highly granular task importance.
- **Outstanding Task Management**: Track overdue or due-today tasks with a dedicated view and quickly navigate through outstanding items.
- **Queue-Based Task Navigation**: Move through your task queue seamlessly, focusing on one task at a time based on priority and due date.

## Why org-queue?

org-queue leverages the powerful capabilities of org-mode to bring a dynamic, queue-based task management system that emphasizes progressive prioritization and scheduling. With org-queue, tasks can be revisited according to a carefully balanced priority system, making it ideal for users who benefit from structured task flow and adaptive scheduling.

## Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/CyberSyntax/org-queue.git
    ```

2. Add `org-queue` to your Emacs configuration by setting up the load path and requiring the package:
    ```emacs-lisp
    ;; Set the path to the org-queue directory
    (setq org-queue-dir "/path/to/org-queue/")
    ;; Add the org-queue directory to Emacs load path
    (add-to-list 'load-path (file-name-as-directory org-queue-dir))

    ;; Load org-queue
    (require 'org-queue)
    ```

Replace `"/path/to/org-queue/"` with the actual path where you cloned the repository. For example, if you cloned it into `~/emacs/org-queue/`, you would set `org-queue-dir` to `"/home/your-username/emacs/org-queue/"`.

3. Start using `org-queue` with your existing org-mode setup!

## Usage

org-queue provides a range of custom commands to help you navigate and manage your task queue efficiently:

- **Queue Navigation**:
  - `C-c q n`: Show the next outstanding task.
  - `C-c q p`: Show the previous outstanding task.
  - `C-c q c`: Show the current outstanding task.
  - `C-c q r`: Reset the queue index to the first outstanding task.

With these commands, you can stay focused on critical tasks and manage your agenda effectively.

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0). See the [LICENSE](./LICENSE) file for details.
