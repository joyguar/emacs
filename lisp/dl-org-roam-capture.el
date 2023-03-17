;;; dl-org-roam-capture.el --- dl-org-roam capture functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dl-org-roam)

;;; Variables

(defgroup dl-org-roam-capture nil
  "Custom variables and library for org-roam capture."
  :group 'dl-org-roam)

(defconst dl-org-roam-capture-meeting-template `("m" "Meeting" entry
                                                  (function dl/org-roam-capture--meeting-target)
                                                  (function dl/org-roam-capture--meeting-template)
                                                  :clock-in t
                                                  :clock-resume t)
                                                  "Dl-Org-Roam capture meeting template."
  )

(defconst dl-org-roam-capture-default-template `("d" "default" plain "%?"
                                                 :target (file+head "%(dl/org-roam-subdir-select)/%<%Y%m%dT%H%M%S>.org"
                                                                    ,(string-join '("#+title: ${title}\n"
                                                                                    "#+filetags:\n"
                                                                                    "#+timestamp: %<%Y%m%dT%H%M%S>\n")))
                                                 :unnarrowed t))

;;; Library

(defun dl/org-roam-capture--meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    (if (org-roam-node-file person)
        (let ((path (org-roam-node-file person))
              (headline "Meetings"))
          (set-buffer (org-capture-target-buffer path))
          ;; Org expects the target file to be in Org mode, otherwise
          ;; it throws an error. However, the default notes files
          ;; should work out of the box. In this case, we switch it to
          ;; Org mode.
          (unless (derived-mode-p 'org-mode)
            (org-display-warning
             (format
              "Capture requirement: switching buffer %S to Org mode"
              (current-buffer)))
            (org-mode))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))k
          (if (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline))
               nil t)
              (beginning-of-line)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " headline "\n")
            (beginning-of-line 0)))
      (let ((path dl-org-inbox-file))
        (set-buffer (org-capture-target-buffer path))
        (org-capture-put-target-region-and-position)
        (widen)))))

(defun dl/org-roam-capture--meeting-template ()
  "Return a template for meeting capture."
  (let ((person (dl/org-roam-node-read-tag "people")))
    (org-capture-put :meeting-person person)
    (if (org-roam-node-file person)
        "* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
              (org-roam-node-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(provide 'dl-org-roam-capture)
;;; dl-org-roam-capture.el ends here
