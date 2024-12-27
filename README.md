# org-queue

**org-queue** is an Emacs package designed to enhance **org-mode** with advanced incremental task management capabilities. By introducing features such as priority-based task handling, dynamic scheduling, and queue navigation, org-queue helps you stay focused and organized. Drawing inspiration from incremental learning techniques, it allows you to prioritize, defer, and manage tasks effectively.

## Key Features

- **Granular Task Prioritization**:  
  Assign priorities on a scale of **1 (highest)** to **64 (lowest)** for precise task ranking.

- **Dynamic and Randomized Scheduling**:  
  Automatically distribute tasks across a specified time frame, ensuring even workload distribution. Randomized prioritization minimizes bias when assigning task importance.

- **Outstanding Task Tracking**:  
  Effortlessly monitor overdue tasks and navigate through tasks due today or in the near future.

- **Queue-Based Navigation with Adjustable Anki Integration**:  
  Work sequentially through a prioritized task queue, maintaining focus on one task at a time. Adjust the frequency of Anki launches relative to tasks displayed, integrating spaced repetition into your workflow at your preferred pace.

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
   ;; Add the path to the org-queue directory
   (add-to-list 'load-path "/path/to/org-queue/")

   ;; Load org-queue
   (require 'org-queue)

   ;; Set up org-mode priority settings to align with org-queue
   (setq org-priority-highest 1)   ;; Highest priority
   (setq org-priority-default 32)  ;; Default priority
   (setq org-priority-lowest 64)   ;; Lowest priority

   ;; Optional: Adjust the default scheduling range
   (setq my-random-schedule-default-months 3)

   ;; Optional: Set the default Anki launch ratio
   (setq my-anki-task-ratio 1)  ;; Default is 1:1 (Anki launched every task)
   ```

   **Note**: Replace `"/path/to/org-queue/"` with the full directory path where you cloned the repository. Examples for different operating systems:

   - **Linux**: `/home/<your-username>/<repository-folder>/org-queue/`  
   - **macOS**: `/Users/<your-username>/<repository-folder>/org-queue/`  
   - **Windows**: `C:/Users/<your-username>/<repository-folder>/org-queue/`  

   Replace `<your-username>` with your actual system username and `<repository-folder>` with the name of the folder where you saved the `org-queue` repository. Use the correct path format for your operating system.

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
     When you create a new task using `C-RET`, `org-queue` identifies the priority ranges that include both `org-priority-default` and `org-priority-lowest`. It then combines these ranges, including any intermediate ranges, to form a continuous set of eligible ranges. From this combined set, `org-queue` randomly selects one range and assigns a random priority within that selected range. Subsequently, the task is scheduled to occur randomly within the next three months. This approach ensures that each new task receives a priority that aligns with your predefined heuristic ranges, promoting a balanced and adaptable workload distribution.
     
---

#### 2. **Interactive Random Scheduling**  
   - **Shortcut**: `C-c q s`  
   - **Description**: Prompts the user to specify an upper limit in months for scheduling the current task. The task will then be scheduled randomly within the range of **1 day** to the specified upper limit in months.  

     When you use this command (`C-c q s`), `org-queue`:

     - **Prompts for Scheduling**: Asks for the upper month limit and schedules the task randomly within the range of **1 day** to the specified number of months.
     - **Prompts for Priority**: Immediately after scheduling, it prompts you to select a priority range (0–9), allowing you to assign a priority to the task interactively.

     #### How it works:
     - This command combines **user input** for flexibility and **random scheduling** for dynamic workload distribution.
     - For example:
       - Input `1` → The task is scheduled randomly within the next **30 days**.
       - Input `3` → The task is scheduled randomly within the next **90 days**.
       - Input `6` → The task is scheduled randomly within the next **180 days**.
       - Input `12` → The task is scheduled randomly within the next **1 year**.
       - Input `24` → The task is scheduled randomly within the next **2 years**.

     - **Priority Setting**:
       - After scheduling, you are prompted to select a priority group (0–9), and the priority is assigned accordingly as described in the **Set Priority with Heuristics** section.

     If no input is provided for the upper month limit, the default (`my-random-schedule-default-months`, which is `3` months) will be used.

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
     - **If no input is provided when invoking the command (`C-c q ,`):**
       - **If the current heading has a priority set**, the command defaults to that priority's group and assigns a random priority within the same range.
       - **If the current heading does not have a priority set**, it defaults to **Group 9 (58–64)**, assigning the task a lower priority for less urgent attention.

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

     ##### Adjustable Anki Launch Frequency

     While navigating tasks, `org-queue` allows you to control how frequently the Anki application is launched. By default, Anki is launched every time you display a new task using `C-c q n`. However, you can adjust this frequency to suit your workflow using the `my-set-anki-task-ratio` function.

     **How to Adjust the Anki Launch Frequency:**

     - **Interactively Set the Ratio**:
       - Invoke the function `my-set-anki-task-ratio` within Emacs.
         - You can do this by typing `M-x my-set-anki-task-ratio` and pressing `Enter`.
       - When prompted, enter a positive integer representing the number of tasks after which Anki should be launched.
         - **Examples**:
           - Enter `1` (default): Anki will be launched every time you display a new task.
           - Enter `2`: Anki will be launched every **second** task.
           - Enter `3`: Anki will be launched every **third** task.
           - Enter `5`: Anki will be launched every **fifth** task.

     - **Example Usage**:
       - **Set Anki to Launch Every Third Task**:
         1. Press `M-x`, type `my-set-anki-task-ratio`, and press `Enter`.
         2. When prompted, enter `3`.
         3. Anki will now launch every third task you display with `C-c q n`.
       - **Adjust Anki Launch Frequency on the Fly**:
         - You can change the frequency at any time by invoking `my-set-anki-task-ratio` again and entering a new ratio.

     **Why Adjust the Anki Launch Frequency:**

     - **Customize Your Workflow**: Tailor the integration of spaced repetition to match your current focus and workload.
     - **Reduce Distractions**: If frequent launching of Anki interrupts your task flow, increase the ratio to have Anki launch less often.
     - **Enhance Learning**: When you want to prioritize reviewing flashcards, decrease the ratio to launch Anki more frequently.

     **Note**: Ensure that Anki is installed and properly configured to be launched from Emacs. The function `my-launch-anki` should correctly point to the Anki executable on your system.

     **Tip**: If you find yourself adjusting the Anki launch frequency often, consider creating a custom keybinding for `my-set-anki-task-ratio` in your Emacs configuration for quicker access.

---

## Configuration

To optimize your experience, you can customize `org-queue` settings in your Emacs configuration file.

### Configure Org-Mode's Priority Settings

Align org-mode's priority settings with `org-queue`:

```emacs-lisp
(setq org-priority-highest 1)   ;; Highest priority
(setq org-priority-default 32)  ;; Default priority
(setq org-priority-lowest 64)   ;; Lowest priority
```

### Adjust the Default Scheduling Range

Set the default number of months for random scheduling:

```emacs-lisp
(setq my-random-schedule-default-months 3)
```

- **Example**:
  - To set the default scheduling range to 6 months, add the following line:

    ```emacs-lisp
    (setq my-random-schedule-default-months 6)
    ```

### Setting the Default Anki Launch Ratio

By default, `org-queue` launches Anki every time you display a new task. If you prefer a different default frequency, you can adjust the `my-anki-task-ratio` variable:

```emacs-lisp
(setq my-anki-task-ratio 1)  ;; Default is 1:1 (Anki launched every task)
```

- **Example**:
  - To set Anki to launch every third task by default, add the following line:

    ```emacs-lisp
    (setq my-anki-task-ratio 3)  ;; Anki will launch every 3 tasks
    ```

**Important Notes:**

- **Loading Order Matters**: Ensure that you have loaded `org-queue` before setting these variables. Place `(require 'org-queue)` before the configuration settings in your Emacs configuration file.
  
  ```emacs-lisp
  ;; Add the path to the org-queue directory
  (add-to-list 'load-path "/path/to/org-queue/")

  ;; Load org-queue
  (require 'org-queue)

  ;; Now set your configurations
  (setq org-priority-highest 1)
  ;; ... and so on
  ```

- **Using `setq`**: Always use `setq` to set or change the value of variables in your configuration file. Do not use `defvar` in your configuration file.

---

## License

`org-queue` is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
Refer to the [LICENSE](./LICENSE) file for detailed terms and conditions.
