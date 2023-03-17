;;; dl-org-roam.el --- org functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-roam-db)
(require 'dl)
(require 'dl-directory)

;;; Variables

(defgroup dl-org-roam nil
  "Variables and library for org-roam extensions."
  :group 'org-roam)

(defcustom dl-org-roam-tags-auto-update t
  "If non-nil, auto update tags in Org-roam buffers upon save."
  :group 'dl-org-roam
  :type 'boolean)

;;; General library

(defun dl/org-roam-contains-todo-p ()
  "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks. The only exception is headings tagged as REFILE."
  (org-element-map
   (org-element-parse-buffer 'headline)
   'headline
   (lambda (h)
     (let ((todo-type (org-element-property :todo-type h)))
       (or (eq 'todo todo-type)
           (and
            (org-element-property :contents-begin h)
            (save-excursion
              (goto-char (org-element-property :contents-begin h))
              (let ((end (save-excursion
                           ;; we must look for active timestamps only
                           ;; before then next heading, even if it's
                           ;; child, but org-element-property
                           ;; :contents-end includes all children
                           (or
                            (re-search-forward org-element-headline-re
                                               (org-element-property :contents-end h)
                                               ':noerror)
                            (org-element-property :contents-end h)))))
                (re-search-forward org-ts-regexp end 'noerror)))))))
   nil 'first-match))

(defun dl/org-roam-tags-add ()
  "Add a tag to current note."
  (interactive)
  (org-with-point-at 1
  (when (call-interactively #'org-roam-tag-add)
    (dl/org-roam-ensure-filetag))))

;;;###autoload
(defun dl/org-roam-ensure-filetag ()
  "Add missing FILETAGS to the current note."
  (interactive)
  ;; add base-dir within dl-org-roam directory as tag
  (let* ((base-dir-tag (dl/org-roam-get-base-dir))
         (original-tags (dl/org-roam-get-tags))
         (tags (append original-tags base-dir-tag)))
    ;; process people
    (when (seq-contains-p tags "people")
      (let ((tag (dl/org-roam--title-as-tag)))
        (unless (seq-contains-p tags tag)
          (setq tags (cons tag tags)))))
    ;; process projects
    (if (dl/org-roam-contains-todo-p)
        (setq tags (cons "agenda" tags))
      (setq tags (remove "agenda" tags)))
    (setq tags (seq-uniq tags))
     ;; update tags if changed
    (when (or (seq-difference tags original-tags)
              (seq-difference original-tags tags))
      (apply #'dl/org-roam-set-tags (seq-uniq tags)))))

;;; Buffer library

(defun dl/org-roam-get-base-dir ()
  (let ((file (buffer-file-name)))
    (seq-filter
     (lambda (x) (not (string-empty-p x)))
     (list (file-name-nondirectory
            (directory-file-name
             (string-remove-prefix
              org-roam-directory
              (file-name-directory file))))))))

(defun dl/org-roam--title-as-tag ()
  "Return title of the current note as tag."
  (concat "@" (s-replace " " "" (org-roam-get-keyword "title"))))

(defun dl/org-roam-get-tags ()
  "Return filetags value in current buffer."
  (dl/org-roam-get-keyword-list "filetags" "[ :]"))

(defun dl/org-roam-set-tags (&rest tags)
  "Set TAGS in current buffer.
If filetags value is already set, replace it."
  (if tags
      (org-roam-set-keyword
       "filetags" (concat ":" (string-join tags ":") ":"))
    (org-roam-erase-keyword "filetags")))

(defun dl/org-roam-get-keyword-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (org-roam-get-keyword name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

(defun dl/org-roam-node-read-tag (tag)
  "Return a completing read list of org-roam node candidates
restricted to those with TAG."
  (org-roam-node-read nil
                      (lambda (node)
                        (member tag (org-roam-node-tags node)))
                      (lambda (completion-a completion-b)
                        (< (length (org-roam-node-title (cdr completion-a)))
                           (length (org-roam-node-title (cdr completion-b)))))))

(defun dl/org-roam-subdir-select ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons
               "."
               (seq-map
                (lambda (p)
                  (string-remove-prefix org-roam-directory p))
                (directory-subdirs org-roam-directory 'recursive)))))
    (concat org-roam-directory (completing-read "Subdir: " dirs nil t))))

;;; Hooks

(defun dl/org-roam-update-tags-on-save-h ()
  "run `dl/org-roam-ensure-filetag' before buffer is saved to its file."
  (when dl-org-roam-tags-auto-update
    (add-hook 'before-save-hook #'dl/org-roam-ensure-filetag nil t)))

(provide 'dl-org-roam)
;;; dl-org-roam.el ends here
