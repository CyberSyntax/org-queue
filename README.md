 > ⚠️ WARNING: USE WITH CAUTION ⚠️
> org-queue is a powerful package that can irreversibly alter your org files.

> Once activated, it modifies task structures and schedules in ways that may not be easily reversible. If you’re not fully familiar with
> how the code works or what it’s doing under the hood, do not use it yet.

> Read the source. Understand the logic. Backup your files.
> You’ve been warned.

---

> **Note**: This README is now updated to reflect the current modular architecture and features. The codebase has been restructured into specialized modules for better organization and maintainability.

# Table of Contents

1.  [Org-queue](#org603541a)
    1.  [Key Features](#orgb3454fe)
        1.  [**Granular Task Prioritization**:](#orgc1e6db7)
        2.  [**Dynamic and Randomized Scheduling**:](#org11aa33a)
        3.  [**Outstanding Task Tracking**:](#org6694c5d)
        4.  [**Queue-Based Navigation with Intelligent SRS Integration**:](#org1afffe2)
        5.  [**Comprehensive Task Processing System**:](#orge859be5)
    2.  [Why Choose org-queue?](#org702feff)
    3.  [Installation](#orgaccf879)
        1.  [**Clone the Repository**:](#orgfce2df7)
        2.  [**Configure org-queue in Emacs**:](#orgccd658b)
        3.  [**Restart Emacs**:](#org620857f)
    4.  [Usage](#org76c3385)
        1.  [Key Commands and Shortcuts](#orgac8b2b7)
    5.  [Modular Architecture](#org-modular-arch)
    6.  [Configuration](#org4490433)
        1.  [Customize the Default Scheduling Range](#orgc7c8ede)
        2.  [Adjust the Scheduling Bias](#orgdc89ca4)
        3.  [Setting the Default Anki Launch Ratio](#orgf492064)
        4.  [Customize Priority Ranges](#org4ad37c4)
        5.  [Important Notes](#org24ad904)
        6.  [Sample Configuration](#org01ed1d7)
        7.  [Customizing via Emacs Interface vs. Configuration File](#org76d0e25)
    7.  [License](#org3ef0d00)


<a id="org603541a"></a>

# Org-queue

**org-queue** is a comprehensive Emacs package that transforms **org-mode** into a sophisticated incremental task management system. Built with a modular architecture, it provides intelligent task prioritization, dynamic scheduling, queue-based navigation, and seamless integration with spaced repetition systems (SRS). Drawing inspiration from SuperMemo and incremental learning methodologies, org-queue enables efficient task processing with advanced content extraction and cloze deletion capabilities.


<a id="orgb3454fe"></a>

## Key Features


<a id="orgc1e6db7"></a>

### **Granular Task Prioritization**:

Assign priorities on a scale of **1 (highest)** to **64 (lowest)** for precise task ranking.


<a id="org11aa33a"></a>

### **Dynamic and Randomized Scheduling**:

Automatically distribute tasks across a specified time frame, ensuring even workload distribution. Randomized prioritization minimizes bias when assigning task importance.


<a id="org6694c5d"></a>

### **Outstanding Task Tracking**:

Effortlessly monitor overdue tasks and navigate through tasks due today or in the near future.


<a id="org1afffe2"></a>

### **Queue-Based Navigation with Intelligent SRS Integration**:

Work sequentially through a prioritized task queue with seamless integration of spaced repetition systems. Features automatic SRS session management, exhaustion detection, and smart Anki launching based on platform detection (Windows, macOS, Android).


<a id="orge859be5"></a>

### **Comprehensive Task Processing System**:

Automatic startup processing including overdue task rescheduling, priority constraint enforcement, duplicate task management, and DONE task cleanup. Uses sophisticated algorithms for priority-based scheduling and maintains task list persistence across sessions.


<a id="org702feff"></a>

## Why Choose org-queue?

`org-queue` transforms **org-mode** into a comprehensive incremental learning and task management system. With its modular architecture, it provides advanced content processing (extracts, cloze deletions), intelligent task prioritization, spaced repetition integration, and sophisticated scheduling algorithms. The system maintains full compatibility with standard org-mode workflows while adding powerful incremental learning capabilities inspired by SuperMemo methodologies.


<a id="orgaccf879"></a>

## Installation


<a id="orgfce2df7"></a>

### **Clone the Repository**:

Open your terminal and run:  
```sh
git clone https://github.com/CyberSyntax/org-queue.git
```


<a id="orgccd658b"></a>

### **Configure org-queue in Emacs**:

Add the following lines to your Emacs configuration file (`.emacs` or `init.el`):

```emacs-lisp
;; 📁 Directory Setup
(setq cache-dir "path/to/cache")
(setq org-agenda-directory "path/to/org-agenda")
(setq org-agenda-files (directory-files-recursively org-agenda-directory "\\.org$"))
(setq org-queue-directory org-agenda-directory)

;; 📦 Optional: Load FSRS (if using)
(when (file-exists-p "path/to/fsrs")
  (add-to-list 'load-path "path/to/fsrs")
  (require 'fsrs)
  (setq my-fsrs-weights
        [0.2228 1.0368 10.4109 10.4109 7.2805 0.1947 2.1963 0.0092 1.3297
         0.0077 0.8288 1.6248 0.2235 0.3794 2.0227 0.2315 1.0000 0.3060 0.4473]))

;; 🔁 Optional: Load org-srs (if using spaced repetition)
(when (file-exists-p "path/to/org-srs")
  (add-to-list 'load-path "path/to/org-srs")
  (require 'org-srs)
  (setq org-srs-review-order-new 'priority)
  (setq org-srs-review-order-review 'priority)
  (setq org-srs-review-learn-ahead-limit nil)

  ;; Optional SRS hooks and keybindings
  (defun org-srs-review-show-rating-after-rate ()
    (when-let ((rating (bound-and-true-p org-srs-review-rating)))
      (message "Rated: %S" rating)
      (sleep-for 0.5)))
  
  (add-hook 'org-srs-review-after-rate-hook 'org-srs-review-show-rating-after-rate)
  
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "<f6>") #'org-srs-review-rate-good)
    (define-key org-mode-map (kbd "<f8>") #'org-srs-review-rate-again)))

;; 📋 Load org-queue (main package)
(add-to-list 'load-path "path/to/org-queue")
(require 'org-queue)

;; ⚙️ Configuration
(setq my-random-schedule-default-months 3)     ;; Default scheduling range
(setq org-queue-preinit-srs nil)               ;; Pre-initialize SRS on startup
```

<a id="org620857f"></a>

### **Restart Emacs**:

Once configured, restart Emacs, and `org-queue` will be ready to use!


<a id="org76c3385"></a>

## Usage

`org-queue` integrates seamlessly with **org-mode**, adding incremental learning and dynamic task management capabilities. Below are the key commands and their corresponding shortcuts:


<a id="orgac8b2b7"></a>

### Key Commands and Shortcuts

1.  **Global Prefix Key**

    -   **Shortcut**: `C-;`
    -   **Description**: All org-queue commands are accessible via the global prefix key `C-;` followed by the specific command key.

2.  **Scheduling and Priority Assignment**

    -   **Shortcut**: `C-; s`
    -   **Description**: Assigns both a schedule and a priority to a heading within the default range of three months. Priority is set automatically using the heuristic system after scheduling.
    

3.  **Interactive Task Scheduling with Mathematical Distribution**

    -   **Shortcut**: `C-; s`
    -   **Description**: Prompts you to specify an upper limit in months for scheduling the current task. The task is then scheduled within the range of **1 day** to the specified upper limit, using a mathematical model that biases the scheduling towards later dates in the range.
        
        When you use this command, `org-queue`:
        
        -   **Prompts for Scheduling**: Asks for the maximum number of months you consider acceptable for scheduling the task. It then schedules the task within **1 day** to the specified time frame, following a mathematically elegant distribution that naturally favors later scheduling dates within the range.
        -   **Automatically Sets Priority**: After scheduling, automatically ensures the task has a priority set using the heuristic system.
    
    1.  How It Works:
    
        -   This command combines **user input** for flexibility with a **mathematical scheduling model** to distribute your workload dynamically and appropriately.
        -   For example:
            -   Input `0.2` → The task is scheduled within the next **6 days**, with a higher likelihood towards the end of the period.
            -   Input `0.5` → The task is scheduled within the next **15 days**, with a higher likelihood towards the end of the period.
            -   Input `1` → The task is scheduled within the next **30 days**, with a higher likelihood towards the end of the period.
            -   Input `3` → The task is scheduled within the next **90 days**, with a higher likelihood towards the end of the period.
            -   Input `6` → The task is scheduled within the next **180 days**, with a higher likelihood towards the end of the period.
            -   Input `12` → The task is scheduled within the next **1 year**, with a higher likelihood towards the end of the period.
            -   Input `24` → The task is scheduled within the next **2 years**, with a higher likelihood towards the end of the period.
        
        If no input is provided for the upper month limit, the default value (`my-random-schedule-default-months`, which is `3` months) will be used.
    
    2.  Mathematical Model:
    
        The scheduling mechanism relies on a **mathematically sound probability distribution** to ensure tasks are more likely to be scheduled towards the end of the specified range, without introducing arbitrary parameters.
        
        **1. Total Number of Days:**
        
        Convert the specified number of months into total days:
        
        ```
        total<sub>days</sub> = months \* 30
        ```
        
        **2. Probability Density Function (PDF):**
        
        A polynomial distribution over the interval `[0, 1]` is used:
        
        ```
        f(x) = (n + 1) \* x<sup>n</sup>
        ```
        
        -   **Normalization:** The coefficient `(n + 1)` ensures that the total probability integrates to 1 over the interval `[0, 1]`.
        
        -   **Choice of `n`:**
            
            -   `n = 0`: Uniform distribution (equal likelihood across the range).
            -   `n = 1`: Quadratic distribution; moderate bias towards later dates.
            -   `n = 2`: Cubic distribution; stronger bias towards later dates.
            
            In the code, **`n = 1`** is used for a quadratic distribution.
        
        **3. Cumulative Distribution Function (CDF):**
        
        The CDF is obtained by integrating the PDF:
        
        ```
        F(x) = x<sup>(n + 1)</sup>
        ```
        
        For `n = 1`:
        
        ```
        F(x) = x<sup>2</sup>
        ```
        
        **4. Inverting the CDF:**
        
        A uniform random variable `u` in `[0, 1]` is used to generate `x`:
        
        ```
        x = u<sup>(1 / (n + 1))</sup>
        ```
        
        For `n = 1`:
        
        ```
        x = sqrt(u)
        ```
        
        This inversion biases `x` towards values closer to 1, favoring later dates.
        
        **5. Calculating the Scheduled Date:**
        
        The number of days ahead is computed as:
        
        ```
        days<sub>ahead</sub> = total<sub>days</sub> \* x
        ```
        
        This ensures that the task is scheduled within the specified range, with a natural bias towards dates further in the future.
        
        -   **Priority Setting**:
            -   After scheduling, you are prompted to select a priority group (0–9), and the priority is assigned accordingly as described in the **Set Priority with Heuristics** section.

4.  **Set Priority with Heuristics**

    -   **Shortcut**: `C-; ,`
    -   **Additional Shortcuts**:
        -   `C-; i`: Increase priority range (move to higher priority group)
        -   `C-; d`: Decrease priority range (move to lower priority group)
    -   **Description**: Inspired by a **community proposal discussed in the YouTube video [Priority heuristics in SuperMemo](<https://www.youtube.com/watch?v=OwV5HPKMrbg>)**, this feature builds on the concept of **priorities** already present in SuperMemo. However, it introduces a heuristic-based approach for assigning priorities dynamically, making the process more flexible and adaptable.
    
    1.  How it works:
    
        -   The command prompts the user to choose a priority group (0–9).
        -   Each group corresponds to a specific numerical range for priorities:  
            -   **Group 0**: Priority between 1–2
            -   **Group 1**: Priority between 2–5
            -   **Group 2**: Priority between 5–12
            -   **Group 3**: Priority between 12–18
            -   **Group 4**: Priority between 18–24
            -   **Group 5**: Priority between 24–30
            -   **Group 6**: Priority between 30–37
            -   **Group 7**: Priority between 37–45
            -   **Group 8**: Priority between 45–58
            -   **Group 9**: Priority between 58–64
        
        -   After selecting a group, the command randomly picks a priority within the specified range and assigns it to the current heading.
    
    2.  Default Behavior:
    
        -   **If no input is provided when invoking the command:**
            -   **If the current heading has a priority set**, the command defaults to that priority's group and assigns a random priority within the same range.
            -   **If the current heading does not have a priority set**, it defaults to **Group 9 (58–64)**, assigning the task a lower priority for less urgent attention.
    
    3.  Origin and Philosophy:
    
        This feature is inspired by a **community proposal for SuperMemo**, as detailed in the YouTube video ["Priority heuristics in SuperMemo"](<https://www.youtube.com/watch?v=OwV5HPKMrbg>). While SuperMemo already features a priority system for incremental learning, this proposal introduced a heuristic-based method to dynamically assign and adjust priorities. 
        
        The heuristic approach simplifies the decision-making process for assigning priorities, making it easier to balance competing tasks and manage workload efficiently. `org-queue` adopts this concept to enhance task management in Emacs, allowing users to flexibly and systematically assign priorities to tasks in line with **incremental learning principles**.
        
        A special thanks to the **SuperMemo community** for sharing this innovative idea and inspiring this feature in `org-queue`.

5.  **Navigate Outstanding Tasks**

    -   **Shortcuts**:  
        -   `C-; f`: Move to the next outstanding task in the queue.
        -   `C-; b`: Move to the previous outstanding task in the queue.
        -   `C-; c`: Show the current outstanding task.
        -   `C-; r`: Remove current task from queue.
        -   `C-; R`: Reset the task queue to start from the first task.
    
    -   **Description**: Provides efficient navigation through the queue of overdue or due-today tasks.
    
    1.  Adjustable Anki Launch Frequency
    
        While navigating tasks, `org-queue` offers two methods to control how frequently the Anki application is launched: fixed ratio and priority-based interpolation.
        
        **Platform-Adaptive Anki Integration:**
        
        -   **Desktop Systems**: Automatically detects and launches Anki application when available
        -   **Android/Termux**: Shows message prompting manual Anki launch
        -   **SRS Integration**: When org-srs is available, seamlessly integrates spaced repetition sessions
        -   **Exhaustion Detection**: Automatically detects when no more SRS reviews are available
        -   **Session Management**: Handles SRS session cleanup and buffer saving automatically

6.  **Content Creation and Processing**

    -   **Shortcuts**:
        -   `C-; x`: Create SuperMemo-style extract from selected text
        -   `C-; X`: Remove all extract blocks from current buffer
        -   `C-; z`: Create cloze deletion from selected text (if org-srs available)

7.  **SRS Integration** (if org-srs is available)

    -   **Shortcuts**:
        -   `C-; 1`: Rate current SRS item as "again" (difficult)
        -   `C-; 3`: Rate current SRS item as "good" (easy)

8.  **View and Navigation Control**

    -   **Shortcuts**:
        -   `C-; w`: Widen buffer and recenter
        -   `C-; n`: Narrow to current subtree
        -   `C-; u`: Show parent heading context

9.  **Structure Editing**

    -   **Shortcuts**:
        -   `C-; W`: Cut current subtree
        -   `C-; Y`: Paste subtree
        -   `C-; D`: Demote subtree (increase level)
        -   `C-; P`: Promote subtree (decrease level)

10. **Optional Integrations**

    -   **Web Tools** (if org-web-tools available):
        -   `C-; l`: Insert link for URL
        -   `C-; I`: Insert web page as org entry
    -   **AI Integration** (if gptel available):
        -   `C-; g`: Start GPT chat session

11. **Automatic Postponement of Overdue Tasks**

    -   **Activation**: Automatically runs at Emacs startup
    -   **Manual Override**: `M-x my-auto-postpone-overdue-tasks` (rarely needed)
    -   **Description**: Automatically manages overdue tasks using priority-based scheduling on Emacs startup.
        
        This feature operates automatically when Emacs starts:
        
        -   **Background Processing**: Silently processes all overdue tasks in your agenda files during Emacs initialization.
        -   **Priority-Based Rescheduling**: Uses each task's priority to determine appropriate future dates.
        -   **Auto-Saving**: Handles file saving automatically before and after processing.
    
    1.  When to Use Manual Override:
    
        -   For users who keep Emacs running for extended periods
        -   When you want to force a complete task review outside the normal startup cycle
        -   If you need to resynchronize task schedules mid-session
        
        The mathematical model and functionality remain the same as described earlier, but the key distinction is that this feature is primarily designed as an automatic startup process rather than a regularly used command.
    
    2.  Mathematical Model:
    
        The postponement mechanism uses **linear interpolation** to calculate scheduling months based on priority values.
        
        **1. Priority Range Mapping:**
        
        The function maps priorities to months using linear interpolation:
        
        ```
        months = max<sub>months</sub> \* (priority - highest<sub>priority</sub>) / (lowest<sub>priority</sub> - highest<sub>priority</sub>)
        ```
        
        **2. Linear Interpolation Formula:**
        
        For a priority p in the range [highest<sub>priority</sub>, lowest<sub>priority</sub>]:
        
        ```
        interpolation<sub>factor</sub> = (p - highest<sub>priority</sub>) / (lowest<sub>priority</sub> - highest<sub>priority</sub>)
        months = max<sub>months</sub> \* interpolation<sub>factor</sub>
        ```
        
        -   **Priority Handling:**
            -   Highest Priority (e.g., 1): Results in minimal postponement
            -   Default Priority (e.g., 32): Results in medium postponement
            -   Lowest Priority (e.g., 64): Results in maximum postponement
            -   Missing Priority: Automatically assigned within default-to-lowest range
        
        The calculated months value is then passed to `my-random-schedule` for final scheduling with added randomness to prevent task clustering.

12. **Advance Task Schedule with Mathematical Adjustment**

    -   **Shortcut**: `C-; a`
    -   **Description**: Advances the schedule of the current Org heading by a calculated number of months. The adjustment decreases with the increasing current schedule weight, meaning tasks scheduled further in the future will be advanced by a smaller amount.
    
    1.  How It Works:
    
        -   The command calculates a new scheduled date by reducing the current schedule weight with a mathematically derived formula.
        -   Uses formula: `f(x) = x - 1 / ln(x + e)` to ensure a decreasing adjustment.
        -   Converts the adjusted months into days and schedules the task without pushing it before the current date.
        
        This function allows users to prioritize tasks that are scheduled too far into the future by bringing them closer to the present where they might need earlier attention.

13. **Postpone Task Schedule with Mathematical Adjustment**

    -   **Shortcut**: `C-; p`
    -   **Description**: Postpones the schedule of the current Org heading by a calculated number of months. The postponement uses an increasing function that makes sure tasks of greater schedule weight are postponed by relatively smaller amounts in comparison to lighter ones.
    
    1.  How It Works:
    
        -   The command calculates a later scheduled date using the function: `f(x) = x + 1 / ln(x + e)`.
        -   This ensures that tasks with initially larger schedule weights are postponed less, relative to when they would occur.
        -   Converts the adjusted months into days and directly reschedules the task.
        
        This feature helps in balancing workload by ensuring tasks aren't excessively postponed and allows users to manage their timeline efficiently through a mathematical approach.

14. **Architecture Notes**
    - **Modular Design**: The codebase is organized into specialized modules:
      - `org-queue.el`: Main entry point and keybindings
      - `org-queue-config.el`: Configuration and core settings
      - `org-queue-tasks.el`: Task management and navigation
      - `org-queue-priority.el`: Priority assignment and management
      - `org-queue-schedule.el`: Scheduling algorithms and functions
      - `org-queue-display.el`: UI, highlighting, and display functions
      - `org-queue-utils.el`: Utility functions and task state management
      - `org-queue-srs-bridge.el`: Integration with spaced repetition systems
    - **Smart Integration**: Automatically detects and integrates with org-srs when available
    - **Cross-Platform**: Supports Windows, macOS, and Linux with platform-specific optimizations
    - **Android Support**: Special handling for Android/Termux environments  

<a id="org4490433"></a>

<a id="org-modular-arch"></a>

## Modular Architecture

Org-queue is built with a modular architecture for maintainability and extensibility:

- **`org-queue.el`** - Main entry point, global keybindings (`C-;`), and module loading
- **`org-queue-config.el`** - Core configuration, priority ranges, and system detection
- **`org-queue-tasks.el`** - Task management, navigation, persistence, and automatic processing
- **`org-queue-priority.el`** - Priority assignment algorithms and heuristic-based selection
- **`org-queue-schedule.el`** - Scheduling algorithms, mathematical distributions, and date calculations
- **`org-queue-display.el`** - UI components, syntax highlighting, content extraction, and Anki integration
- **`org-queue-utils.el`** - Utility functions, task state management, and mathematical helpers
- **`org-queue-srs-bridge.el`** - Spaced repetition system integration and SRS session management

This modular design allows for:
- **Easy maintenance** - Each module has a specific responsibility
- **Optional dependencies** - SRS and web tools are gracefully handled when not available
- **Platform adaptability** - Android and desktop differences are handled appropriately
- **Extensibility** - New features can be added without affecting existing functionality

<a id="org4490433"></a>

## Configuration

To optimize your experience, you can customize `org-queue` settings in your Emacs configuration file or through the Emacs customization interface.


<a id="orgc7c8ede"></a>

### Customize the Default Scheduling Range

Set the default number of months for random scheduling by customizing `my-random-schedule-default-months`. This variable controls how far into the future tasks are scheduled when no specific value is provided.

1.  Option 1: Using Emacs Customization Interface

    1.  Run `M-x customize-variable RET my-random-schedule-default-months RET`.
    2.  Set the desired number of months (e.g., `6` for six months).
    3.  Save your changes to persist them across sessions.

2.  Option 2: Setting in Emacs Configuration File

    Add the following line to your Emacs configuration file (`init.el` or `.emacs`):
    
    ```emacs-lisp
    (setq my-random-schedule-default-months 3)
    ```
    
    -   **Example**:
        -   To set the default scheduling range to **6 months**, add:
            
            ```emacs-lisp
            (setq my-random-schedule-default-months 6)
            ```


<a id="orgdc89ca4"></a>

### Adjust the Scheduling Bias

You can control the bias of the scheduling distribution towards later dates by customizing `my-random-schedule-exponent`. This variable defines the exponent `n` in the mathematical model used for scheduling.

-   **Values**:
    -   `0`: Uniform distribution (no bias).
    -   `1`: Quadratic distribution (default; moderate bias towards later dates).
    -   `2`: Cubic distribution (stronger bias towards later dates).

1.  Option 1: Using Emacs Customization Interface

    1.  Run `M-x customize-variable RET my-random-schedule-exponent RET`.
    2.  Set the desired exponent value (e.g., `2` for a stronger bias).
    3.  Save your changes.

2.  Option 2: Setting in Emacs Configuration File

    Add the following line to your configuration file:
    
    ```emacs-lisp
    (setq my-random-schedule-exponent 1)  ;; Default value
    ```
    
    -   **Example**:
        -   To use a cubic distribution for stronger bias towards later dates, add:
            
            ```emacs-lisp
            (setq my-random-schedule-exponent 2)
            ```


<a id="orgf492064"></a>

### SRS Integration Settings

Org-queue provides intelligent integration with spaced repetition systems when available.

1.  Option 1: Using Emacs Customization Interface

    1.  Run `M-x customize-variable RET org-queue-preinit-srs RET`.
    2.  Enable to pre-initialize SRS on startup.
    3.  Save your changes.

2.  Option 2: Setting in Emacs Configuration File

    Add the following line to your configuration file:
    
    ```emacs-lisp
    (setq org-queue-preinit-srs t)  ;; Pre-initialize SRS on startup
    ```


<a id="org4ad37c4"></a>

### Customize Priority Ranges

If you wish to adjust how priorities are assigned within `org-queue`, you can customize `my-priority-ranges`. This variable contains the mapping of priority ranges.

1.  Using Emacs Customization Interface

    1.  Run `M-x customize-variable RET my-priority-ranges RET`.
    2.  Edit the priority ranges as desired.
    3.  Save your changes.
    
    **Note**: Editing `my-priority-ranges` requires understanding of how priorities are mapped. Each entry is a cons cell where the key is the range identifier, and the value is a cons cell of minimum and maximum priority values.


<a id="org24ad904"></a>

### Important Notes

-   **Loading Order Matters**:
    -   Ensure that you have loaded `org-queue` before setting these variables. Place `(require 'org-queue)` before the configuration settings in your Emacs configuration file.
        
        ```emacs-lisp
        ;; Add the path to the org-queue directory
        (add-to-list 'load-path "*path/to/org-queue*")
        
        ;; Load org-queue
        (require 'org-queue)
        
        ;; Now set your configurations
        (setq org-priority-highest 1)
        ;; … and so on
        ```

-   **Customizable Variables**:
    -   Variables like `my-random-schedule-default-months`, `my-random-schedule-exponent`, `org-queue-preinit-srs`, and `my-priority-ranges` are defined using `defcustom`. You can customize them via the Emacs customization interface or set them using `setq` in your configuration file.

-   **Using `setq`**:
    -   To set or change the value of these variables in your configuration file, use `setq`. Avoid using `defvar` or `defcustom` in your configuration file, as they are meant for variable definitions within the package code.

-   **Emacs Customization Interface**:
    -   Access the Emacs customization interface by running `M-x customize-group RET org-queue RET`. This interface allows you to view and modify all customizable variables related to `org-queue`.


<a id="org01ed1d7"></a>

### Sample Configuration

Here is how you might set up your Emacs configuration file with `org-queue`:

```emacs-lisp
;; Add the path to the org-queue directory
(add-to-list 'load-path "*path/to/org-queue*")

;; Load org-queue
(require 'org-queue)

;; Customize org-queue variables
(setq my-random-schedule-default-months 6)   ;; Default scheduling range of 6 months
(setq my-random-schedule-exponent 2)         ;; Use cubic distribution for scheduling
(setq org-queue-preinit-srs t)               ;; Pre-initialize SRS on startup (optional)

;; Optionally, customize priority ranges (only if you need to adjust defaults)
;; (setq my-priority-ranges
;;       '((0 . (1 . 2))
;;         ;; … your custom ranges …
;;         (9 . (58 . 64))))

;; Optional: Set up automatic task processing hooks
;; (add-hook 'org-insert-heading-hook #'my-post-org-insert-heading)
```

**Note**: Replace `"*path/to/org-queue*"` with the actual path to where `org-queue` is located on your system.


<a id="org76d0e25"></a>

### Customizing via Emacs Interface vs. Configuration File

-   **Customization Interface**:
    -   Pros:
        -   User-friendly, no need to write code.
        -   Variables are documented and explained.
    -   Cons:
        -   Changes are saved in a separate file (`custom.el`), which may not be under version control.

-   **Configuration File (`init.el` or `.emacs`)**:
    -   Pros:
        -   All configurations are in one place.
        -   Easy to version control.
    -   Cons:
        -   Requires familiarity with Emacs Lisp.

Choose the method that best fits your workflow.


<a id="org3ef0d00"></a>

## License

`org-queue` is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
Refer to the [LICENSE](./LICENSE) file for detailed terms and conditions.
