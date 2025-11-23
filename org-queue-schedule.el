;;; org-queue-schedule.el --- Scheduling functions for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)
(require 'org-queue-tasks)

;;; --- FILE-FIRST SCHEDULING INVARIANTS (single path; no options) ---
;; Always-on rules (no user options here):
;; - Snap to local work window [09:00, 18:00).
;; - Apply a small ±10-minute jitter (kept inside the window).
;; - Respect LAST_REPEAT deferral (priority-based).

(defconst org-queue--avoid-weekends nil)
(defconst org-queue--work-window-start "09:00")
(defconst org-queue--work-window-end   "18:00")
(defconst org-queue--schedule-jitter-minutes 10)

(defun org-queue--minutes-of-day (tm)
  (let* ((d (decode-time tm)) (h (nth 2 d)) (m (nth 1 d)))
    (+ (* 60 h) m)))

(defun org-queue--date-at-local-midnight (tm)
  (let* ((d (decode-time tm)) (y (nth 5 d)) (mo (nth 4 d)) (da (nth 3 d)))
    (encode-time 0 0 0 da mo y)))

(defun org-queue--add-days (tm n)
  (time-add tm (days-to-time n)))

(defun org-queue--weekend-p (tm)
  (let ((dow (nth 6 (decode-time tm)))) ;; 0=Sun .. 6=Sat
    (or (= dow 0) (= dow 6))))

(defun org-queue--parse-hhmm-safe (s fallback)
  (or (org-queue--parse-hhmm s) (org-queue--parse-hhmm fallback) 0))

(defun org-queue--snap-to-work-window (tm)
  "Return a time snapped into the work window, respecting weekend avoidance.
Rules:
- If TM falls on a weekend and weekend avoidance is enabled, move it to the
  next working day at the start of the window.
- Otherwise, if TM is outside the window, move it to the next valid slot
  (start of window today if before start; start of window next day if after end)."
  (let* ((start (org-queue--parse-hhmm-safe org-queue--work-window-start "09:00"))
         (end   (org-queue--parse-hhmm-safe org-queue--work-window-end   "18:00"))
         (start<=end (<= start end))
         (x tm))
    ;; 1) Push weekends to next working day at window start (EARLY transform).
    (when (and org-queue--avoid-weekends (org-queue--weekend-p x))
      (let ((y x))
        (while (org-queue--weekend-p y)
          (setq y (org-queue--add-days y 1)))
        (setq x (time-add (org-queue--date-at-local-midnight y)
                          (seconds-to-time (* 60 start))))))
    ;; 2) Snap into the window on the (possibly transformed) day.
    (let* ((mid (org-queue--date-at-local-midnight x))
           (hm  (org-queue--minutes-of-day x)))
      (if start<=end
          (cond
           ((< hm start) (time-add mid (seconds-to-time (* 60 start))))
           ((>= hm end)  (time-add (org-queue--add-days mid 1)
                                   (seconds-to-time (* 60 start))))
           (t x))
        ;; Window spans midnight (rare)
        (if (or (>= hm start) (< hm end))
            x
          (time-add mid (seconds-to-time (* 60 start))))))))

(defun org-queue--apply-scheduling-constraints (candidate)
  "Adjust CANDIDATE by invariants:
- >= now,
- >= LAST_REPEAT + deferral(priority) for non-SRS entries,
- snapped to work window (and next working day),
- ±10 min jitter kept inside the window (and not before now)."
  (let* ((now (current-time))
         (cand (if (time-less-p candidate now) now candidate))
         ;; Respect LAST_REPEAT deferral (non-SRS only; we are at point)
         (last (org-queue--parse-last-repeat))
         (priority (ignore-errors (my-get-raw-priority-value)))
         (def-min (and (numberp priority) (org-queue--deferral-minutes priority)))
         (cand (if (and last def-min)
                   (let ((min-time (time-add last (seconds-to-time (* 60 def-min)))))
                     (if (time-less-p cand min-time) min-time cand))
                 cand))
         ;; Snap into the window
         (cand (org-queue--snap-to-work-window cand))
         ;; Bounded jitter
         (j (max 0 org-queue--schedule-jitter-minutes))
         (cand (if (> j 0)
                   (let* ((delta-min (- (random (1+ (* 2 j))) j))
                          (cand2 (time-add cand (seconds-to-time (* 60 delta-min))))
                          (cand2 (org-queue--snap-to-work-window cand2)))
                     ;; Ensure never before NOW after jitter
                     (if (time-less-p cand2 now)
                         (org-queue--snap-to-work-window now)
                       cand2))
                 cand)))
    cand))

(defun org-queue--schedule-set! (tm)
  "Write SCHEDULED with all constraints; then micro-update+save."
  (let* ((final (org-queue--apply-scheduling-constraints tm)))
    (org-schedule nil (format-time-string "%Y-%m-%d %a %H:%M" final))
    (ignore-errors (org-queue--micro-update-current! 'schedule))
    (when org-queue-auto-show-top-after-change
      (org-queue-show-top t))
    final))

(defcustom my-random-schedule-default-months 3
  "Default number of months to schedule if none is specified."
  :type 'integer
  :group 'org-queue)

(defcustom my-random-schedule-exponent 1
  "Exponent n controlling the bias of the scheduling distribution.
- n = 0: Uniform distribution (no bias).
- n = 1: Quadratic distribution (default).
- n = 2: Cubic distribution (stronger bias towards later dates)."
  :type 'integer
  :group 'org-queue)

(defun my-find-schedule-weight ()
  "Calculate schedule weight based on SCHEDULED date in org header.
Returns:
- 0 for past dates and today
- Number of months ahead (days/30.0) for future dates
- `my-random-schedule-default-months` if no SCHEDULED date exists."
  (let* ((scheduled-time (org-get-scheduled-time (point)))  ; Get the SCHEDULED time
         (current-time (current-time))                      ; Get the current time
         (days-difference
          (when scheduled-time
            ;; Calculate the difference in days, then convert to months
            (/ (float (- (time-to-days scheduled-time)
                         (time-to-days current-time)))
               30.0))))
    (cond
     ((null scheduled-time)
      ;; If no SCHEDULED time, return the default months
      (or (bound-and-true-p my-random-schedule-default-months) 0))
     ((<= days-difference 0)
      ;; If the difference is 0 or negative, return 0
      0)
     (t
      ;; Otherwise, return the rounded difference in months to 2 decimal places
      (my-round-to-decimals days-difference 2)))))

(defun my-random-schedule (months &optional n)
  "Schedule MONTHS months in the future using a power-law distribution.
Priority enhancement: higher priority biases toward earlier dates.
Skips scheduling if the current heading or its parent has an SRS drawer."
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if heading or parent has SRS drawer
    (let ((srs-result (org-srs-entry-p (point))))
      (if (or (eq srs-result 'current) (eq srs-result 'parent))
          ;; Skip scheduling if entry or parent has SRS drawer
          (message "Skipping scheduling for entry with SRS drawer")
        ;; Proceed with normal scheduling
        (let* ((today (current-time))
               (total-days (* months 30))
               ;; Get priority for bias calculation
               (priority-str (org-entry-get nil "PRIORITY"))
               (priority (if priority-str
                             (string-to-number priority-str)
                           org-priority-default))
               ;; Priority-based bias: high priority increases exponent for earlier bias
               (priority-ratio (/ (float (- org-priority-lowest priority))
                                  (float (- org-priority-lowest org-priority-highest))))
               (priority-bias (+ 1.0 (* 2.0 priority-ratio)))  ; Range: 1.0 to 3.0
               ;; Existing mathematical logic with priority enhancement
               (base-n (or n my-random-schedule-exponent))
               (enhanced-n (* base-n priority-bias))           ; Priority enhances the exponent
               (u (/ (float (random 1000000)) 1000000.0))
               (exponent (/ 1.0 (+ enhanced-n 1)))             ; compute 1/(enhanced_n+1)
               (x (expt u exponent))
               (days-ahead (floor (* total-days x)))
               (raw (time-add today (days-to-time days-ahead))))
          ;; Single scheduling path with invariants + micro-update + save
          (org-queue--schedule-set! raw)
          (message "Scheduled: Priority %d (bias %.2fx) → %d days"
                   priority priority-bias days-ahead))))))

(defun my-random-schedule-command (&optional months)
  "Interactive command to schedule MONTHS months in the future.
If MONTHS is not provided, uses the result of my-find-schedule-weight."
  (interactive
   (list (read-number
          "Enter the upper month limit: "
          (my-find-schedule-weight))))
  (save-excursion
    ;; Schedule the current heading
    (my-random-schedule (or months (my-find-schedule-weight)))))

(defun my-advance-schedule ()
  "Advance the current Org heading by a mathematically adjusted number of months.
Uses priority-based factor multiplied with existing mathematical logic.
Does not schedule tasks to dates before today.
Skips scheduling if the current heading or its parent is an SRS entry."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    (unless (org-srs-entry-p (point)) ; Only continue if NOT an SRS entry
      (let* ((e (exp 1))
             (current-weight (max 0 (my-find-schedule-weight)))
             (priority-str (org-entry-get nil "PRIORITY"))
             (priority (if priority-str
                           (string-to-number priority-str)
                         org-priority-default))
             (priority-ratio (/ (float (- org-priority-lowest priority))
                                (float (- org-priority-lowest org-priority-highest))))
             (priority-factor (+ 0.1 (* 0.9 priority-ratio)))
             (base-adjusted-months (max 0 (- current-weight
                                             (/ 1 (log (+ current-weight e))))))
             (random-months (* base-adjusted-months priority-factor))
             (adjusted-days (* random-months 30.4375))
             (raw (time-add (current-time) (days-to-time adjusted-days))))
        ;; Single scheduling path with invariants + micro-update + save
        (org-queue--schedule-set! raw)
        (message "Advanced: Priority %d (factor %.2f) → %.2f months"
                 priority priority-factor random-months)))))

(defun my-postpone-schedule ()
  "Postpone the current Org heading by a mathematically adjusted number of months.
Uses priority-based factor multiplied with existing mathematical logic.
Skip postponing if the current entry or its parent contains an SRS drawer."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    (let ((srs-status (org-srs-entry-p (point))))
      (unless srs-status
        (let* ((e (exp 1))
               (current-weight (max 0 (my-find-schedule-weight)))
               (is-overdue (my-is-overdue-task))
               (priority-str (org-entry-get nil "PRIORITY"))
               (priority (if priority-str
                             (string-to-number priority-str)
                           org-priority-default))
               (priority-ratio (/ (float (- priority org-priority-highest))
                                  (float (- org-priority-lowest org-priority-highest))))
               (priority-factor (+ 0.1 (* 0.9 priority-ratio)))
               (base-adjusted-months (+ current-weight
                                        (/ 1 (log (+ current-weight e)))))
               (random-months (* base-adjusted-months priority-factor))
               (adjusted-days (* random-months 30.4375))
               (now (current-time))
               (proposed-new-time (time-add now (days-to-time adjusted-days)))
               (minimum-time (if is-overdue
                                 now
                               (let* ((now-decoded (decode-time now))
                                      (year (nth 5 now-decoded))
                                      (month (nth 4 now-decoded))
                                      (day (nth 3 now-decoded)))
                                 (encode-time 0 0 0 (1+ day) month year))))
               (raw (if (time-less-p proposed-new-time minimum-time)
                        minimum-time
                      proposed-new-time)))
          ;; Single scheduling path with invariants + micro-update + save
          (org-queue--schedule-set! raw)
          (message "Postponed: Priority %d (factor %.2f) → %.2f months%s"
                   priority priority-factor random-months
                   (if is-overdue " (overdue→today allowed)" "")))))))

;; --- Stamp-aware wrappers (single save; final 'stamp only if stamped)
(defun org-queue-advance-schedule-and-stamp ()
  "Advance schedule, then stamp LAST_REPEAT (single save)."
  (interactive)
  (my-advance-schedule)
  (let ((st (ignore-errors (org-queue-stamp-last-repeat-current))))
    (when (eq st :stamped)
      (ignore-errors (org-queue--micro-update-current! 'stamp)))))

(defun org-queue-postpone-schedule-and-stamp ()
  "Postpone schedule, then stamp LAST_REPEAT (single save)."
  (interactive)
  (my-postpone-schedule)
  (let ((st (ignore-errors (org-queue-stamp-last-repeat-current))))
    (when (eq st :stamped)
      (ignore-errors (org-queue--micro-update-current! 'stamp)))))

(defun my-auto-advance-schedules (&optional power)
  "Advance 2^POWER random tasks (default: 64) across org-queue files."
  (interactive "P")
  (let* ((n (or power 6))
         (limit (expt 2 n))
         (candidates (org-queue-collect-markers))
         (shuffled (my-custom-shuffle candidates))
         (total (length shuffled))
         (processed 0)
         (count 0))
    (let ((org-queue--suppress-ui t))
      (catch 'break
        (dolist (m shuffled)
          (when (>= count limit) (throw 'break nil))
          (setq count (1+ count))
          (org-with-point-at m
            (my-advance-schedule)
            (setq processed (1+ processed))))))
    (message "Advanced %d/%d (2^%d=%d)" processed total n limit)))

(defun my-auto-postpone-schedules (&optional power)
  "Postpone 2^POWER random tasks (default: 64) across org-queue files."
  (interactive "P")
  (let* ((n (or power 6))
         (limit (expt 2 n))
         (candidates (org-queue-collect-markers))
         (shuffled (my-custom-shuffle candidates))
         (total (length shuffled))
         (processed 0)
         (count 0))
    (let ((org-queue--suppress-ui t))
      (catch 'break
        (dolist (m shuffled)
          (when (>= count limit) (throw 'break nil))
          (setq count (1+ count))
          (org-with-point-at m
            (my-postpone-schedule)
            (setq processed (1+ processed))))))
    (message "Postponed %d/%d (2^%d=%d)" processed total n limit)))

(defun my-schedule-command (&optional months)
  "Interactive command that schedules MONTHS months in the future and prompts for priority."
  (interactive
   (list (read-number
          "Enter the upper month limit: "
          (my-find-schedule-weight))))
  (let ((orig-buf (current-buffer)))
    (my-random-schedule (or months (my-find-schedule-weight)))
    (my-set-priority-with-heuristics)
    (my-ensure-priority-set)
    (ignore-errors (org-queue--micro-update-current! 'schedule))))

;; --- Schedule → priority (reuse) → stamp. Single save.
(defun org-queue-schedule-and-prioritize (&optional months)
  "Schedule (prompt for months), then interactively choose a priority range. Single save + LAST_REPEAT stamp."
  (interactive
   (list (read-number
          "Enter the upper month limit: "
          (my-find-schedule-weight))))
  ;; Schedule (keeps invariants)
  (my-random-schedule (or months (my-find-schedule-weight)))
  ;; Priority prompt (identical UX to pressing \",\")
  (call-interactively 'my-set-priority-with-heuristics)
  ;; Keep property consistent (no-op if already correct)
  (ignore-errors (my-ensure-priority-set))
  ;; Stamp LAST_REPEAT (no-op on SRS/parent) and reflect a final 'stamp update
  (let ((st (ignore-errors (org-queue-stamp-last-repeat-current))))
    (when (eq st :stamped)
      (ignore-errors (org-queue--micro-update-current! 'stamp)))))

(defun org-queue-stamp-last-repeat-current ()
  "Stamp :LAST_REPEAT: on the current heading only if it’s a pure non-SRS entry.
Returns a status symbol: :stamped, :skipped-not-org, or :skipped-srs.
Never signals on those skip cases."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (progn (message "Not in an org-mode buffer; skipping LAST_REPEAT stamp")
             :skipped-not-org)
    (save-excursion
      (org-back-to-heading t)
      ;; Disallow when SRS-related (current or parent)
      (let ((where (org-srs-entry-p (point))))
        (if where
            (progn
              (message "SRS-related entry (%s): LAST_REPEAT stamping is disabled here"
                       (symbol-name where))
              :skipped-srs)
          ;; Stamp LAST_REPEAT on this heading
          (let* ((now     (current-time))
                 (now-str (format-time-string "[%Y-%m-%d %a %H:%M]" now)))
            (org-entry-put nil "LAST_REPEAT" now-str)
            (let ((verify (org-entry-get nil "LAST_REPEAT")))
              (unless (and verify (string= (string-trim verify) now-str))
                (org-set-property "LAST_REPEAT" now-str)))
            (message "Stamped LAST_REPEAT %s on current heading" now-str)
            :stamped))))))

(defun my-ensure-priorities-and-schedules-for-all-headings (&optional max-attempts)
  "Ensure priorities and schedules are set for all headings across Org agenda files.
Repeatedly processes headings until all have priorities and schedules, or max-attempts is reached.
Skip DONE tasks entirely.
MAX-ATTEMPTS: Maximum number of retry attempts (defaults to 15)."
  (interactive)
  (let ((max-attempts (or max-attempts 15))
        (attempt 0)
        (all-complete nil)
        (processed-headings (make-hash-table :test 'equal)))

    (while (and (not all-complete) (< attempt max-attempts))
      (setq attempt (1+ attempt))

      ;; First pass: Count total entries and incomplete entries
      (let ((total-entries 0)
            (incomplete-entries 0))
        (org-queue-map-entries
         (lambda ()
           (setq total-entries (1+ total-entries))
           ;; Skip DONE tasks entirely
           (unless (my-is-done-task)
             ;; First check if entry has missing priority or schedule
             (when (or (not (org-entry-get nil "PRIORITY"))
                       (string= (org-entry-get nil "PRIORITY") " ")
                       (not (org-entry-get nil "SCHEDULED")))
               ;; Only now check if it's an SRS entry (more expensive operation)
               (let* ((marker (point-marker))
                      (file (buffer-file-name))
                      (position (point))
                      (heading-id (concat file ":" (number-to-string position)))
                      (srs-status (or (gethash heading-id processed-headings)
                                      (puthash heading-id (org-srs-entry-p (point)) processed-headings))))
                 ;; Count as incomplete based on SRS status:
                 ;; - 'parent: skip both priority and schedule (never incomplete)
                 ;; - 'current: only incomplete if missing priority (we skip scheduling)
                 ;; - nil: incomplete if missing either
                 (cond
                  ((eq srs-status 'parent)
                   nil) ; Skip entirely
                  ((eq srs-status 'current)
                   ;; Only check priority for 'current entries
                   (when (or (not (org-entry-get nil "PRIORITY"))
                             (string= (org-entry-get nil "PRIORITY") " "))
                     (setq incomplete-entries (1+ incomplete-entries))))
                  (t
                   ;; For non-SRS entries, count if missing either
                   (setq incomplete-entries (1+ incomplete-entries))))))))
         nil)

        ;; Process entries if there are incomplete ones
        (when (> incomplete-entries 0)
          (org-queue-map-entries
           (lambda ()
             ;; Skip DONE tasks entirely
             (unless (my-is-done-task)
               ;; First check if entry has missing priority or schedule
               (when (or (not (org-entry-get nil "PRIORITY"))
                         (string= (org-entry-get nil "PRIORITY") " ")
                         (not (org-entry-get nil "SCHEDULED")))
                 ;; Only check SRS status if needed
                 (let* ((file (buffer-file-name))
                        (position (point))
                        (heading-id (concat file ":" (number-to-string position)))
                        (srs-status (or (gethash heading-id processed-headings)
                                        (puthash heading-id (org-srs-entry-p (point)) processed-headings))))
                   (cond
                    ;; For 'parent entries, skip entirely
                    ((eq srs-status 'parent)
                     nil)
                    ;; For 'current entries, only set priority if needed
                    ((eq srs-status 'current)
                     (condition-case err
                         (let ((current-priority (org-entry-get nil "PRIORITY")))
                           (when (or (not current-priority)
                                     (string= current-priority " "))
                             (my-ensure-priority-set)))
                       (error
                        (message "Error processing priority for 'current entry: %s"
                                 (error-message-string err)))))
                    ;; For non-SRS entries, process both priority and schedule
                    (t
                     (condition-case err
                         (progn
                           ;; Ensure priority is set only if missing
                           (let ((current-priority (org-entry-get nil "PRIORITY")))
                             (when (or (not current-priority)
                                       (string= current-priority " "))
                               (my-ensure-priority-set)))
                           ;; Ensure schedule is set only if missing
                           (unless (org-entry-get nil "SCHEDULED")
                             (my-random-schedule (my-find-schedule-weight) 0)))
                       (error
                        (message "Error processing entry: %s"
                                 (error-message-string err))))))))))
           nil))

        ;; Set all-complete if no incomplete entries found
        (setq all-complete (zerop incomplete-entries)))

      (message "Attempt %d/%d completed. %s"
               attempt
               max-attempts
               (if all-complete
                   "All entries processed successfully!"
                 "Some entries are still incomplete.")))

    (when (and (not all-complete) (>= attempt max-attempts))
      (message "Warning: Reached maximum attempts (%d). Some entries may still be incomplete."
               max-attempts))))

(provide 'org-queue-schedule)
;;; org-queue-schedule.el ends here
