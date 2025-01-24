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

- **Automatic Overdue Task Management**:
Smart rescheduling of overdue tasks on Emacs startup using priority-based linear interpolation. Higher priority tasks are rescheduled more urgently while lower priority tasks are given more flexible future dates, ensuring optimal workload distribution.

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

#### 1. **Scheduling and Priority Assignment**
   - **Shortcut**: `C-RET`
   - **Description**: Assigns both a schedule and a priority to a new heading within the default range of three months.

     #### Task Scheduling and Prioritization
     When you create a new task using `C-RET`, the task is both scheduled to occur randomly within the next three months and assigned a priority. Upon invocation, the command prompts the user to select a priority group (0–9). Each group corresponds to a specific numerical range for priorities, allowing the user to define the range for the task's priority.

     Once the user selects the priority group, `org-queue` randomly assigns a numeric priority within that group. Simultaneously, the task is scheduled to occur randomly within the next three months.

---

#### 2. **Interactive Task Scheduling with Mathematical Distribution**  
   - **Shortcut**: `C-c q s`  
   - **Description**: Prompts you to specify an upper limit in months for scheduling the current task. The task is then scheduled within the range of **1 day** to the specified upper limit, using a mathematical model that biases the scheduling towards later dates in the range.

     When you use this command (`C-c q s`), `org-queue`:

     - **Prompts for Scheduling**: Asks for the maximum number of months you consider acceptable for postponing the task. It then schedules the task within **1 day** to the specified time frame, following a mathematically elegant distribution that naturally favors later scheduling dates within the range.
     - **Prompts for Priority**: Immediately after scheduling, it prompts you to select a priority range (0–9), allowing you to assign a priority to the task interactively.

     #### How It Works:
     - This command combines **user input** for flexibility with a **mathematical scheduling model** to distribute your workload dynamically and appropriately.
     - For example:
       - Input `0.2` → The task is scheduled within the next **6 days**, with a higher likelihood towards the end of the period.
       - Input `0.5` → The task is scheduled within the next **15 days**, with a higher likelihood towards the end of the period.
       - Input `1` → The task is scheduled within the next **30 days**, with a higher likelihood towards the end of the period.
       - Input `3` → The task is scheduled within the next **90 days**, with a higher likelihood towards the end of the period.
       - Input `6` → The task is scheduled within the next **180 days**, with a higher likelihood towards the end of the period.
       - Input `12` → The task is scheduled within the next **1 year**, with a higher likelihood towards the end of the period.
       - Input `24` → The task is scheduled within the next **2 years**, with a higher likelihood towards the end of the period.

     If no input is provided for the upper month limit, the default value (`my-random-schedule-default-months`, which is `3` months) will be used.

     #### Mathematical Model:

     The scheduling mechanism relies on a **mathematically sound probability distribution** to ensure tasks are more likely to be scheduled towards the end of the specified range, without introducing arbitrary parameters.

     **1. Total Number of Days:**

     Convert the specified number of months into total days:

     ```
     total_days = months * 30
     ```

     **2. Probability Density Function (PDF):**

     A polynomial distribution over the interval `[0, 1]` is used:

     ```
     f(x) = (n + 1) * x^n
     ```

     - **Normalization:** The coefficient `(n + 1)` ensures that the total probability integrates to 1 over the interval `[0, 1]`.

     - **Choice of `n`:**

       - `n = 0`: Uniform distribution (equal likelihood across the range).
       - `n = 1`: Quadratic distribution; moderate bias towards later dates.
       - `n = 2`: Cubic distribution; stronger bias towards later dates.

       In the code, **`n = 1`** is used for a quadratic distribution.

     **3. Cumulative Distribution Function (CDF):**

     The CDF is obtained by integrating the PDF:

     ```
     F(x) = x^(n + 1)
     ```

     For `n = 1`:

     ```
     F(x) = x^2
     ```

     **4. Inverting the CDF:**

     A uniform random variable `u` in `[0, 1]` is used to generate `x`:

     ```
     x = u^(1 / (n + 1))
     ```

     For `n = 1`:

     ```
     x = sqrt(u)
     ```

     This inversion biases `x` towards values closer to 1, favoring later dates.

     **5. Calculating the Scheduled Date:**

     The number of days ahead is computed as:

     ```
     days_ahead = total_days * x
     ```

     This ensures that the task is scheduled within the specified range, with a natural bias towards dates further in the future.

     - **Priority Setting**:
       - After scheduling, you are prompted to select a priority group (0–9), and the priority is assigned accordingly as described in the **Set Priority with Heuristics** section.

---

#### 3. **Set Priority with Heuristics**  
   - **Shortcut**: `C-c q ,`
   - **Additional Shortcuts**:
     - `C-c q i`: Increase priority range (move to higher priority group)
     - `C-c q d`: Decrease priority range (move to lower priority group)
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

     While navigating tasks, `org-queue` offers two methods to control how frequently the Anki application is launched: fixed ratio and priority-based interpolation.

     **Priority-Based Interpolation Mode:**

     - **Enable Dynamic Ratio Adjustment**:
       - The launch frequency automatically adjusts based on task priority
       - Lower priority tasks trigger Anki more frequently
       - The ratio is interpolated between:
         - Highest priority (1): Launches at the maximum ratio
         - Lowest priority (64): Launches every task

     **Why Use Priority-Based Interpolation:**

     - **Adaptive Learning**: Automatically increases Anki exposure for low-priority tasks
     - **Dynamic Workflow**: Adjusts automatically without manual intervention

     **Fixed Ratio Mode:**

     - **Set a Fixed Launch Ratio**:
       - Use `M-x my-set-anki-task-ratio` and enter a positive integer.
       - This disables priority-based interpolation and uses a fixed ratio.
         - **Examples**:
           - Enter `1`: Launch Anki every task
           - Enter `3`: Launch Anki every third task
           - Enter `5`: Launch Anki every fifth task

---


#### 5. **Automatic Postponement of Overdue Tasks**
   - **Activation**: Automatically runs at Emacs startup
   - **Manual Override**: `M-x my-auto-postpone-overdue-tasks` (rarely needed)
   - **Description**: Automatically manages overdue tasks using priority-based scheduling on Emacs startup.

     This feature operates automatically when Emacs starts:

     - **Background Processing**: Silently processes all overdue tasks in your agenda files during Emacs initialization.
     - **Priority-Based Rescheduling**: Uses each task's priority to determine appropriate future dates.
     - **Auto-Saving**: Handles file saving automatically before and after processing.

     #### When to Use Manual Override:
     - For users who keep Emacs running for extended periods
     - When you want to force a complete task review outside the normal startup cycle
     - If you need to resynchronize task schedules mid-session

     The mathematical model and functionality remain the same as described earlier, but the key distinction is that this feature is primarily designed as an automatic startup process rather than a regularly used command.

     #### Mathematical Model:

     The postponement mechanism uses **linear interpolation** to calculate scheduling months based on priority values.

     **1. Priority Range Mapping:**

     The function maps priorities to months using linear interpolation:

     ```
     months = max_months * (priority - highest_priority) / (lowest_priority - highest_priority)
     ```

     **2. Linear Interpolation Formula:**

     For a priority p in the range [highest_priority, lowest_priority]:

     ```
     interpolation_factor = (p - highest_priority) / (lowest_priority - highest_priority)
     months = max_months * interpolation_factor
     ```

     - **Priority Handling:**
       - Highest Priority (e.g., 1): Results in minimal postponement
       - Default Priority (e.g., 32): Results in medium postponement
       - Lowest Priority (e.g., 64): Results in maximum postponement
       - Missing Priority: Automatically assigned within default-to-lowest range

     The calculated months value is then passed to `my-random-schedule` for final scheduling with added randomness to prevent task clustering.
     
---

#### 6. **Advance Task Schedule with Mathematical Adjustment**  
   - **Shortcut**: `C-c q a`  
   - **Description**: Advances the schedule of the current Org heading by a calculated number of months. The adjustment decreases with the increasing current schedule weight, meaning tasks scheduled further in the future will be advanced by a smaller amount.

     #### How It Works:
     - The command calculates a new scheduled date by reducing the current schedule weight with a mathematically derived formula.
     - Uses formula: `f(x) = x - 1 / ln(x + e)` to ensure a decreasing adjustment.
     - Converts the adjusted months into days and schedules the task without pushing it before the current date.

     This function allows users to prioritize tasks that are scheduled too far into the future by bringing them closer to the present where they might need earlier attention.

---

#### 7. **Postpone Task Schedule with Mathematical Adjustment**  
   - **Shortcut**: `C-c q p`  
   - **Description**: Postpones the schedule of the current Org heading by a calculated number of months. The postponement uses an increasing function that makes sure tasks of greater schedule weight are postponed by relatively smaller amounts in comparison to lighter ones.

     #### How It Works:
     - The command calculates a later scheduled date using the function: `f(x) = x + 1 / ln(x + e)`.
     - This ensures that tasks with initially larger schedule weights are postponed less, relative to when they would occur.
     - Converts the adjusted months into days and directly reschedules the task.

     This feature helps in balancing workload by ensuring tasks aren't excessively postponed and allows users to manage their timeline efficiently through a mathematical approach.

---

## Configuration

To optimize your experience, you can customize `org-queue` settings in your Emacs configuration file or through the Emacs customization interface.

### Customize the Default Scheduling Range

Set the default number of months for random scheduling by customizing `my-random-schedule-default-months`. This variable controls how far into the future tasks are scheduled when no specific value is provided.

#### Option 1: Using Emacs Customization Interface

1. Run `M-x customize-variable RET my-random-schedule-default-months RET`.
2. Set the desired number of months (e.g., `6` for six months).
3. Save your changes to persist them across sessions.

#### Option 2: Setting in Emacs Configuration File

Add the following line to your Emacs configuration file (`init.el` or `.emacs`):

```emacs-lisp
(setq my-random-schedule-default-months 3)
```

- **Example**:
  - To set the default scheduling range to **6 months**, add:

    ```emacs-lisp
    (setq my-random-schedule-default-months 6)
    ```

### Adjust the Scheduling Bias

You can control the bias of the scheduling distribution towards later dates by customizing `my-random-schedule-exponent`. This variable defines the exponent `n` in the mathematical model used for scheduling.

- **Values**:
  - `0`: Uniform distribution (no bias).
  - `1`: Quadratic distribution (default; moderate bias towards later dates).
  - `2`: Cubic distribution (stronger bias towards later dates).

#### Option 1: Using Emacs Customization Interface

1. Run `M-x customize-variable RET my-random-schedule-exponent RET`.
2. Set the desired exponent value (e.g., `2` for a stronger bias).
3. Save your changes.

#### Option 2: Setting in Emacs Configuration File

Add the following line to your configuration file:

```emacs-lisp
(setq my-random-schedule-exponent 1)  ;; Default value
```

- **Example**:
  - To use a cubic distribution for stronger bias towards later dates, add:

    ```emacs-lisp
    (setq my-random-schedule-exponent 2)
    ```

### Setting the Default Anki Launch Ratio

By default, `org-queue` launches Anki every time you display a new task. To change the frequency, customize `my-anki-task-ratio`.

#### Option 1: Using Emacs Customization Interface

1. Run `M-x customize-variable RET my-anki-task-ratio RET`.
2. Set the desired ratio.
3. Save your changes.

#### Option 2: Setting in Emacs Configuration File

Add the following line to your configuration file:

```emacs-lisp
(setq my-anki-task-ratio 1)  ;; Default is 1:1 (Anki launched every task)
```

- **Example**:
  - To set Anki to launch every third task, add:

    ```emacs-lisp
    (setq my-anki-task-ratio 3)
    ```

### Customize Priority Ranges

If you wish to adjust how priorities are assigned within `org-queue`, you can customize `my-priority-ranges`. This variable contains the mapping of priority ranges.

#### Using Emacs Customization Interface

1. Run `M-x customize-variable RET my-priority-ranges RET`.
2. Edit the priority ranges as desired.
3. Save your changes.

**Note**: Editing `my-priority-ranges` requires understanding of how priorities are mapped. Each entry is a cons cell where the key is the range identifier, and the value is a cons cell of minimum and maximum priority values.

### Important Notes

- **Loading Order Matters**:
  - Ensure that you have loaded `org-queue` before setting these variables. Place `(require 'org-queue)` before the configuration settings in your Emacs configuration file.

    ```emacs-lisp
    ;; Add the path to the org-queue directory
    (add-to-list 'load-path "/path/to/org-queue/")

    ;; Load org-queue
    (require 'org-queue)

    ;; Now set your configurations
    (setq org-priority-highest 1)
    ;; ... and so on
    ```

- **Customizable Variables**:
  - Variables like `my-random-schedule-default-months`, `my-random-schedule-exponent`, `my-anki-task-ratio`, and `my-priority-ranges` are defined using `defcustom`. You can customize them via the Emacs customization interface or set them using `setq` in your configuration file.

- **Using `setq`**:
  - To set or change the value of these variables in your configuration file, use `setq`. Avoid using `defvar` or `defcustom` in your configuration file, as they are meant for variable definitions within the package code.

- **Emacs Customization Interface**:
  - Access the Emacs customization interface by running `M-x customize-group RET org-queue RET`. This interface allows you to view and modify all customizable variables related to `org-queue`.

### Sample Configuration

Here is how you might set up your Emacs configuration file with `org-queue`:

```emacs-lisp
;; Add the path to the org-queue directory
(add-to-list 'load-path "/path/to/org-queue/")

;; Load org-queue
(require 'org-queue)

;; Customize org-queue variables
(setq my-random-schedule-default-months 6)   ;; Default scheduling range of 6 months
(setq my-random-schedule-exponent 2)         ;; Use cubic distribution for scheduling
(setq my-anki-task-ratio 8)

;; Optionally, customize priority ranges (only if you need to adjust defaults)
;; (setq my-priority-ranges
;;       '((0 . (1 . 2))
;;         ;; ... your custom ranges ...
;;         (9 . (58 . 64))))
```

**Note**: Replace `"/path/to/org-queue/"` with the actual path to where `org-queue` is located on your system.

### Customizing via Emacs Interface vs. Configuration File

- **Customization Interface**:
  - Pros:
    - User-friendly, no need to write code.
    - Variables are documented and explained.
  - Cons:
    - Changes are saved in a separate file (`custom.el`), which may not be under version control.

- **Configuration File (`init.el` or `.emacs`)**:
  - Pros:
    - All configurations are in one place.
    - Easy to version control.
  - Cons:
    - Requires familiarity with Emacs Lisp.

Choose the method that best fits your workflow.

---

## License

`org-queue` is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
Refer to the [LICENSE](./LICENSE) file for detailed terms and conditions.
