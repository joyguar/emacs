;;; config-org.el -*- lexical-binding: t; -*-

;;; Builtin

(setup org
  (setq org-list-allow-alphabetical t) ;; Must be set before org is loaded
  (:require org dl-org)
  (:option org-directory dl-org-root-dir
           org-adapt-indentation nil
           org-startup-indented t
           org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
           org-property-format "%-24s %s"
           org-use-fast-todo-selection t
           org-todo-keywords '((sequence "TODO(t!)" "[ ](c)" "NEXT(n!)" "|" "DONE(d)")
                               (sequence "WAIT(w@/!)" "HOLD(h@/!)" "SOMEDAY(s)" "|" "KILL(k@/!)" "MEETING"))
           org-todo-keyword-faces '(("TODO" :foreground "goldenrod2" :weight bold)
                                    ("NEXT" :foreground "dodger blue" :weight bold)
                                    ("DONE" :foreground "medium sea green" :weight bold)
                                    ("WAIT" :foreground "coral" :weight bold)
                                    ("HOLD" :foreground "medium orchid" :weight bold)
                                    ("SOMEDAY" :foreground "medium turquoise" :weight bold)
                                    ("KILL" :foreground "indian red" :weight bold)
                                    ("MEETING" :foreground "papaya whip" :weight bold))
           org-todo-state-tags-triggers '(("KILL" ("KILL" . t))
                                          ("WAIT" ("SOMEDAY") ("HOLD") ("WAIT" . t))
                                          ("HOLD" ("WAIT") ("SOMEDAY") ("HOLD" . t))
                                          ("SOMEDAY" ("WAIT") ("HOLD") ("SOMEDAY" . t))
                                          (done ("WAIT") ("HOLD") ("SOMEDAY"))
                                          ("TODO" ("WAIT") ("KILL") ("HOLD") ("SOMEDAY"))
                                          ("NEXT" ("WAIT") ("KILL") ("HOLD") ("SOMEDAY"))
                                          ("DONE" ("WAIT") ("KILL") ("HOLD") ("SOMEDAY")))
           org-log-done 'time
           org-log-into-drawer t
           org-enforce-todo-dependencies t
           org-columns-default-format (concat "%40ITEM(Task) "
                                              "%Effort(EE){:} "
                                              "%CLOCKSUM(Time Spent) "
                                              "%SCHEDULED(Scheduled) "
                                              "%DEADLINE(Deadline)")
           org-tags-column 0
           org-tag-alist '(("@errand" . ?e)
                           ("@home" . ?h)
                           ("@transit" . ?t)
                           ("journals" . ?j))
           org-use-tag-inheritance t
           org-tags-exclude-from-inheritance '("people",
                                               "agenda")
           org-format-latex-options '( :foreground default
                                       :background default
                                       :scale 2.75
                                       :html-foreground "Black"
                                       :html-background "Transparent"
                                       :html-scale 1.0
                                       :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(setup org-element
  (:option org-element-use-cache nil))

(setup org-keys
  (:option org-return-follows-link t))

(setup org-cycle
  (:option org-cycle-separator-lines 1))

(setup org-fold
  (:option org-fold-catch-invisible-edits 'smart))

(setup ox-latex
  (:option org-latex-prefer-user-labels t))

(setup org-id
  (:require dl-org)
  (:option org-id-locations-file dl-org-id-file))

(setup org-capture
  (:require dl-org-capture)
  (:option org-capture-bookmark nil
           org-capture-templates `(,dl-org-capture-inbox-template
                                   ,dl-org-capture-slipbox-template)))

(setup org-agenda
  (:require dl-org-agenda)
  (:option org-agenda-clockreport-parameter-plist '( :link t
                                                     :maxlevel 6
                                                     :fileskip 0 t
                                                     :compact t
                                                     :score 0)
           org-agenda-tags-column 'auto
           org-agenda-sticky nil
           org-agenda-inhibit-startup nil
           org-agenda-dim-blocked-tasks nil
           org-agenda-compact-blocks nil
           org-agenda-time-grid (quote ((daily today remove-match)
                                        (800 1000 1200 1400 1600 1800 2000)
                                        "......" "----------------"))
           org-agenda-tags-todo-honor-ignore-options t
           org-deadline-warning-days 14
           org-agenda-prefix-format '((agenda . " %i %(dl/org-agenda-category 12)%?-12t% s")
                                      (todo . " %i %(dl/org-agenda-category 12) ")
                                      (tags . " %i %(dl/org-agenda-category 12) ")
                                      (search . " %i %(dl/org-agenda-category 12) "))
           org-agenda-window-setup 'current-window)
  (advice-add 'org-agenda :before #'dl/org-agenda-files-update))

(setup org-clock
  (:option org-clock-into-drawer "CLOCK"
           org-clock-idle-time 15
           org-time-stamp-rounding-minutes (quote (0 5))
           org-clock-out-when-done t
           org-clock-out-remove-zero-time-clocks t
           org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory)
           org-clock-persist t)
  (org-clock-persistence-insinuate))

(setup oc
  (:require dl-bibliography)
  (:option org-cite-global-bibliography `(,dl-bibliography-dir)
           org-cite-export-processors '((md . (csl "chicago-full-note-bibliography.csl"))
                                        (latex . biblatex)
                                        (odt . (csl "chicago-fullnote-bibliography.csl"))
                                        (t . (csl "modern-language-association.csl")))))

;;; External

(setup (:elpaca org-modern)
  (:option org-modern-star '("✸" "○" "●" "☙" "✿")
           org-modern-table t
           org-modern-block-fringe t
           org-modern-todo nil
           org-modern-timestamp nil
           org-modern-hide-stars nil
           org-modern-block-name nil
           org-modern-keyword nil
           org-modern-priority nil
           org-modern-tag nil
           org-modern-statistics nil
           org-modern-checkbox nil)
  (:hook-into org-mode))

(setup (:elpaca (org-modern-indent :host github :repo "jdtsmith/org-modern-indent"))
  (:hook-into org-mode))

(setup (:elpaca org-ql)
  (:require dl-org-ql)
  (:option org-agenda-custom-commands `(("p" "Primary View"
                                         ((agenda)
                                          ,dl-org-ql-block-refile
                                          ,dl-org-ql-block-next-tasks
                                          ,dl-org-ql-block-projects
                                          ,dl-org-ql-block-project-tasks
                                          ,dl-org-ql-block-standalone-tasks
                                          ,dl-org-ql-block-waiting-tasks
                                          ,dl-org-ql-block-next-tasks
                                          ,dl-org-ql-block-held-tasks
                                          ,dl-org-ql-block-someday-tasks
                                          ,dl-org-ql-block-dead-tasks)))))

(setup (:elpaca org-roam)
  (:require dl-org-roam)
  (:option org-roam-directory dl-org-root-dir
           org-roam-dailies-directory (expand-file-name "journals" dl-org-writings-dir)
           org-roam-database-connector 'sqlite-builtin
           org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory)
           org-roam-completion-everywhere t
           org-roam-node-display-template (concat "${title:*}"
                                                  (propertize "${tags:20}" 'face 'org-tag))
           org-roam-db-autosync-mode t
           dl-org-roam-tags-auto-update t
           org-roam-capture-templates `(,dl-org-roam-capture-default-template))
  (:long-hook org-roam-find-file-hook #'dl/org-roam-update-tags-on-save-h)
  (:global "C-c r f" org-roam-node-find
           "C-c r i" org-roam-node-insert))

(setup org-roam-capture
  (:when-loaded
    (:require dl-org-roam-capture)
    (add-to-list 'org-capture-templates dl-org-roam-capture-meeting-template)) ;; add this list in whole, not element-wise
  )

(setup (:elpaca citar)
  (:option org-cite-insert-processor 'citar
           org-cite-follow-processor 'citar
           org-cite-activate-processor 'citar
           citar-bibliography org-cite-global-bibliography
           citar-notes-paths `(,dl-org-refs-dir)
           citar-templates '((main . "${author editor:18}     ${date year issued:4}     ${title:30}")
                             (suffix . "          ${=key= id:14}    ${=type=:8}    ${tags keywords:*}")
                             (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                             (note . "Notes on ${author editor}, ${title}"))))

(setup (:elpaca citar-org-roam)
  (:option citar-org-roam-mode t))

(setup flashcards
  (:elpaca (anki-editor :host github :repo "orgtre/anki-editor"))
  (:require dl-org-flashcards)
  (:option anki-editor-create-decks t
           anki-editor-org-tags-as-anki-tags t)
  (add-to-list 'org-capture-templates dl-org-flashcards-capture-template))

(provide 'config-org)
;;; config-org.el ends here
