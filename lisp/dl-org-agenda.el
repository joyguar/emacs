;;; dl-org-agenda.el --- dl-org-agenda functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-habit)

(require 'dl-org)


(defvar dl/org-agenda-hide-scheduled-and-waiting-next-tasks t
  "Non-nil means to hide scheduled and waiting tasks.

Affects the following commands:

- `dl/org-agenda-cmd-project-next-tasks'
- `dl/org-agenda-cmd-project-subtasks'
- `dl/org-agenda-cmd-standalone-tasks'
- `dl/org-agenda-cmd-waiting-postponed-tasks'")

(defun dl/org-agenda-main ()
  "Show main `org-agenda' view."
  (interactive)
  (org-agenda nil "p"))

(defun dl/org-agenda-person ()
  (interactive)
  (let* ((person (org-roam-node-read
                  nil
                  (lambda (node)
                    (member "people" (org-roam-node-tags node)))
                  (lambda (completion-a completion-b)
                    (< (length (org-roam-node-title (cdr completion-a)))
                       (length (org-roam-node-title (cdr completion-b)))))
                  t))
         (names (cons (org-roam-node-title person)
                      (org-roam-node-aliases person)))
         (tags (seq-map (org-roam-get-keyword "title") names))
         (query (string-join tags "|")))
    (dlet ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))

(defun dl/org-project-files ()
  "Return a list of note files containing \\='agenda' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote "%\"agenda\"%"))]))))

(defun dl/org-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (seq-uniq (append (list dl-org-inbox-file)
                                           (dl/org-project-files)))))

;;;; Agenda commands

(defconst dl-org-agenda-cmd-today-schedule
  '(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
               (org-agenda-span 'day)
               (org-agenda-ndays 1)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+0d")))
  "A block showing a 1 day schedule.")

(defconst dl-org-agenda-cmd-weekly-log
  '(agenda "" ((org-agenda-overriding-header "Weekly Log")))
  "A block showing my schedule and logged tasks for this week.")

(defconst dl-org-agenda-cmd-previous-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Previous Calendar Data (last 3 weeks)")
               (org-agenda-start-day "-21d")
               (org-agenda-span 21)
               (org-agenda-start-on-weekday nil)))
  "A block showing my schedule and logged tasks for the last few weeks.")

(defconst dl-org-agenda-cmd-upcoming-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Upcoming Calendar Data (next 2 weeks)")
               (org-agenda-start-day "0d")
               (org-agenda-span 14)
               (org-agenda-start-on-weekday nil)))
  "A block showing my schedule for the next couple weeks.")

(defconst dl-org-agenda-cmd-refile
  '(tags "REFILE"
         ((org-agenda-overriding-header "To refile")
          (org-tags-match-list-sublevels nil))))

(defconst dl-org-agenda-cmd-stuck-projects
  '(tags-todo "-CANCELLED-SOMEDAY/!"
              ((org-agenda-overriding-header "Stuck Projects")
               (org-agenda-skip-function 'dl/org-agenda-skip-non-stuck-projects)
               (org-agenda-sorting-strategy '(category-keep)))))

(defconst dl-org-agenda-cmd-projects
  '(tags-todo "-HOLD-CANCELLED-SOMEDAY/!"
              ((org-agenda-overriding-header "Projects")
               (org-agenda-skip-function 'dl/org-agenda-skip-non-projects)
               (org-tags-match-list-sublevels 'indended)
               (org-agenda-sorting-strategy '(category-keep)))))

(defconst dl-org-agenda-cmd-project-next-tasks
  '(tags-todo "-CANCELLED-SOMEDAY/!NEXT"
              ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                     (if dl/org-agenda-hide-scheduled-and-waiting-next-tasks
                                                         ""
                                                       " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'dl/org-agenda-skip-projects-and-habits-and-single-tasks)
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-with-date dl/org-agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy '(category-keep)))))

(defconst dl-org-agenda-cmd-project-subtasks
   '(tags-todo "-REFILE-CANCELLED-WAITING-HOLD-SOMEDAY/!"
              ((org-agenda-overriding-header (concat "Project Subtasks"
                                                     (if dl/org-agenda-hide-scheduled-and-waiting-next-tasks
                                                         ""
                                                       " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'dl/org-agenda-skip-non-project-tasks)
               (org-agenda-todo-ignore-with-date dl/org-agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy '(category-keep)))))

(defconst dl-org-agenda-cmd-standalone-tasks
  '(tags-todo "-REFILE-CANCELLED-WAITING-HOLD-SOMEDAY/!"
             ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                    (if dl/org-agenda-hide-scheduled-and-waiting-next-tasks
                                                        ""
                                                      " (including WAITING and SCHEDULED tasks)")))
              (org-agenda-skip-function 'dl/org-agenda-skip-project-tasks)
              (org-agenda-todo-ignore-with-date dl/org-agenda-hide-scheduled-and-waiting-next-tasks)
              (org-agenda-sorting-strategy '(category-keep)))))

(defconst dl-org-agenda-cmd-waiting-postponed-tasks
  '(tags-todo "-CANCELLED+WAITING|HOLD/!"
             ((org-agenda-overriding-header (concat "Waiting Tasks"
                                                    (if dl/org-agenda-hide-scheduled-and-waiting-next-tasks
                                                        ""
                                                      " (include WAITING and SCHEDULED tasks)")))
              (org-agenda-skip-function 'dl/org-agenda-skip-non-tasks)
              (org-tags-match-list-sublevels nil)
              (org-agenda-todo-ignore-scheduled dl/org-agenda-hide-scheduled-and-waiting-next-tasks)
              (org-agenda-todo-ignore-deadlines dl/org-agenda-hide-scheduled-and-waiting-next-tasks))))

(defconst dl-org-agenda-cmd-someday-projects-and-tasks
  '(tags-todo "SOMEDAY"
              ((org-agenda-overriding-header "Someday Projects and Tasks")
               (org-tags-match-list-sublevels nil))))


;; Utilities to build agenda comands -- skip

(defun dl/org-agenda-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (dl/org-agenda-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun dl/org-agenda-skip-non-projects ()
  "Skip trees that are not projects"
  (if (save-excursion (dl/org-agenda-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond ((dl/org-agenda-project-p)
                 nil)
                ((and (dl/org-agenda-project-subtree-p) (not (dl/org-agenda-task-p)))
                 nil)
                (t
                 subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun dl/org-agenda-skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond ((dl/org-agenda-task-p)
             nil)
            (t
             next-headline)))))

(defun dl/org-agenda-skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond ((dl/org-agenda-project-p)
             subtree-end)
            ((org-is-habit-p)
             subtree-end)
            (t
             nil)))))

(defun dl/org-agenda-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond ((org-is-habit-p)
             next-headline)
            ((and dl/org-agenda-hide-scheduled-and-waiting-next-tasks
                  (member "WAITING" (org-get-tags)))
             next-headline)
            ((dl/org-agenda-project-p)
             next-headline)
            ((and (dl/org-agenda-task-p) (not (dl/org-agenda-project-subtree-p)))
             next-headline)
            (t
             nil)))))

(defun dl/org-agenda-skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond ((dl/org-agenda-project-p)
             subtree-end)
            ((org-is-habit-p)
             subtree-end)
            ((dl/org-agenda-project-subtree-p)
             subtree-end)
            (t
             nil)))))

(defun dl/org-agenda-skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond ((dl/org-agenda-project-p)
             next-headline)
            ((org-is-habit-p)
             subtree-end)
            ((and (dl/org-agenda-project-subtree-p)
                  (member (org-get-todo-state) (list "NEXT")))
             subtree-end)
            ((not (dl/org-agenda-project-subtree-p))
             subtree-end)
            (t
             nil)))))

(defun dl/org-agenda-skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (dl/org-agenda-subproject-p)
        nil
      next-headline)))


;; Utilities to build agenda commands -- predicates

(defun dl/org-agenda-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun dl/org-agenda-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (dl/org-agenda-find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun dl/org-agenda-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun dl/org-agenda-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))


;; Utilities to build agenda commands -- search

(defun dl/org-agenda-find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;; Utilities to process agenda entries

(defun dl/org-agenda-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "REFILE")
  (dl/org-agenda-bulk-process-entries))

(defvar dl/org-agenda-current-effort "1:00"
  "Current effort for agenda items.")

(defun dl/org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " dl/org-agenda-current-effort) nil nil dl/org-agenda-current-effort)))
  (setq dl/org-agenda-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-fold-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil dl/org-agenda-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun dl/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun dl/org-agenda-bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'dl/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun dl/org-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        \\='((agenda . \" %(org-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (org-roam-get-keyword "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))


(provide 'dl-org-agenda)
;;; dl-org-agenda.el ends here
