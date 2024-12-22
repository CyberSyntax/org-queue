# org-queue

**org-queue** is an Emacs package designed to enhance **org-mode** with advanced incremental task management capabilities. By introducing features such as priority-based task handling, dynamic scheduling, and queue navigation, org-queue helps you stay focused and organized. Drawing inspiration from incremental learning techniques, it allows you to prioritize, defer, and manage tasks effectively.

## Key Features

- **Granular Task Prioritization**:  
  Assign priorities on a scale of **1 (highest)** to **64 (lowest)** for precise task ranking.

- **Dynamic and Randomized Scheduling**:  
  Automatically distribute tasks across a specified time frame, ensuring even workload distribution. Randomized prioritization minimizes bias when assigning task importance.

- **Outstanding Task Tracking**:  
  Effortlessly monitor overdue tasks and navigate through tasks due today or in the near future.

- **Queue-Based Navigation**:  
  Work sequentially through a prioritized task queue, maintaining focus on one task at a time.

---

## Why Choose org-queue?

`org-queue` builds upon the powerful foundation of **org-mode** to provide a robust and flexible task management system. By combining features like structured queues, randomization, and progressive scheduling, it enables users to manage tasks efficiently while maintaining flexibility and control.

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
   Once configured, restart Emacs, and `org-queue` will be ready to use!

---

## Usage

`org-queue` integrates seamlessly with **org-mode**, adding incremental learning and dynamic task management capabilities. Below are the key commands and their corresponding shortcuts:

---

### Key Commands and Shortcuts

#### 1. **Automatic Priority Assignment**  
   - **Shortcut**: `C-RET`  
   - **Description**: Automatically assigns a random priority and schedules a new heading within the default range of three months.  

     #### Task Prioritization
     Tasks in `org-queue` are assigned priorities based on the following scale:  
     - **Highest Priority**: `1`  
     - **Default Priority**: Defined by `org-priority-default` (default: `32`)  
     - **Lowest Priority**: Defined by `org-priority-lowest` (default: `64`)  

     When you create a new task (e.g., with `C-RET`) or perform interactive random scheduling (e.g., with `C-c q s`), `org-queue` automatically assigns a random priority within the range defined by `org-priority-default` and `org-priority-lowest`, provided the task does not already have a priority.

     This ensures a flexible system that adapts to your workflow while maintaining a balanced workload distribution.

---

#### 2. **Interactive Random Scheduling**  
   - **Shortcut**: `C-c q s`  
   - **Description**: Prompts the user to specify an upper limit in months for scheduling the current task. The task will then be scheduled randomly within the range of **1 day** to the specified upper limit in months.  

     If the current heading does not already have a priority, a random priority will be assigned automatically after scheduling.

     #### How it works:
     - This command combines **user input** for flexibility and **random scheduling** for dynamic workload distribution.
     - For example:
       - Input `1` → The task is scheduled randomly within the next **30 days**.
       - Input `3` → The task is scheduled randomly within the next **90 days**.
       - Input `6` → The task is scheduled randomly within the next **180 days**.
       - Input `12` → The task is scheduled randomly within the next **1 year**.
       - Input `24` → The task is scheduled randomly within the next **2 years**.

     If no input is provided, the default upper limit (`my-random-schedule-default-months`, which is `3` months) will be used.

---

#### 3. **Set Priority with Heuristics**  
   - **Shortcut**: `C-c q ,`  
   - **Description**: Inspired by a **community proposal discussed in the YouTube video [Priority heuristics in SuperMemo](https://www.youtube.com/watch?v=OwV5HPKMrbg)**, this feature builds on the concept of **priorities** already present in SuperMemo. However, it introduces a heuristic-based approach for assigning priorities dynamically, making the process more flexible and adaptable.

     #### How it works:
     - The command prompts the user to choose a priority group (0–9).  
     - Each group corresponds to a specific numerical range for priorities:  
       - **Group 0**: Priority between 1–2  
       - **Group 1**: Priority between 2–5  
       - **Group 2**: Priority between 5–12  
       - **Group 3**: Priority between 12–18  
       - **Group 4**: Priority between 18–24  
       - **Group 5**: Priority between 24–30  
       - **Group 6**: Priority between 30–37  
       - **Group 7**: Priority between 37–45  
       - **Group 8**: Priority between 45–58  
       - **Group 9**: Priority between 58–64  

     - After selecting a group, the command randomly picks a priority within the specified range and assigns it to the current heading.

     #### Default Behavior:
     - If no input is provided, it defaults to **Group 9 (58–64)**, assigning the task a lower priority for less urgent attention.

     #### Origin and Philosophy:
     This feature is inspired by a **community proposal for SuperMemo**, as detailed in the YouTube video ["Priority heuristics in SuperMemo"](https://www.youtube.com/watch?v=OwV5HPKMrbg). While SuperMemo already features a priority system for incremental learning, this proposal introduced a heuristic-based method to dynamically assign and adjust priorities. 

     The heuristic approach simplifies the decision-making process for assigning priorities, making it easier to balance competing tasks and manage workload efficiently. `org-queue` adopts this concept to enhance task management in Emacs, allowing users to flexibly and systematically assign priorities to tasks in line with **incremental learning principles**.

     A special thanks to the **SuperMemo community** for sharing this innovative idea and inspiring this feature in `org-queue`.

---

#### 4. **Navigate Outstanding Tasks**  
   - **Shortcut**:  
     - `C-c q n`: Move to the next outstanding task in the queue.  
     - `C-c q p`: Move to the previous outstanding task in the queue.  
     - `C-c q c`: Show the current outstanding task.  
     - `C-c q r`: Reset the task queue to start from the first task.  

   - **Description**: Provides efficient navigation through the queue of overdue or due-today tasks.  

     #### Bonus Feature: Launch Anki
     During the navigation (`C-c q n`), if the **Anki** application is installed, it will automatically launch to complement the task workflow with flashcard-based incremental learning.

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

## License

`org-queue` is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
Refer to the LICENSE file for detailed terms and conditions.
