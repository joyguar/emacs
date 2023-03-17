;;; dl-org-ql.el --- dl-org-ql functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-ql)

;;; Variables

(defgroup dl-org-ql nil
  "Variables and library for org-ql extensions."
  :group 'org-ql)

(defconst dl-org-ql-block-refile
  `(org-ql-block '(and (todo "TODO"
                             "MEETING")
                       (tags "REFILE"))
                 ((org-ql-block-header "REFILE"))))

(defconst dl-org-ql-block-projects
  '(org-ql-block '(and (todo "TODO")
                        (descendants (todo)))
                  ((org-ql-block-header "PROJECTS"))))

(defconst dl-org-ql-block-project-tasks
  '(org-ql-block '(and (todo "TODO")
                        (not (descendants (todo)))
                        (ancestors (todo)))
                  ((org-ql-block-header "PROJECT TASKS"))))

(defconst dl-org-ql-block-standalone-tasks
  '(org-ql-block '(and (todo "TODO")
                        (not (descendants (todo)))
                        (not (ancestors (todo)))
                        (not (tags "REFILE")))
                  ((org-ql-block-header "STANDALONE TASKS"))))

(defconst dl-org-ql-block-waiting-tasks
  '(org-ql-block '(and (todo "WAIT"))
                  ((org-ql-block-header "WAITING TASKS"))))

(defconst dl-org-ql-block-next-tasks
  '(org-ql-block '(and (todo "NEXT"))
                  ((org-ql-block-header "NEXT TASKS"))))

(defconst dl-org-ql-block-held-tasks
  '(org-ql-block '(and (todo "HOLD"))
                  ((org-ql-block-header "HELD TASKS"))))

(defconst dl-org-ql-block-dead-tasks
  '(org-ql-block '(and (todo "DONE" "KILL"))
                 ((org-ql-block-header "DEAD TASKS"))))

(defconst dl-org-ql-block-someday-tasks
  '(org-ql-block '(and (todo "SOMEDAY"))
                 ((org-ql-block-header "SOMEDAY TASKS"))))

(defconst dl-org-ql-block-archive
  '(org-ql-block '(and (todo "DONE"))
                 ((org-ql-block0header "ARCHIVE"))))

(provide 'dl-org-ql)
;;; dl-org-ql.el ends here
