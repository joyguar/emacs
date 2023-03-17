;;; dl-org-flashcards.el --- dl-org-flashcards functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dl-org)

;;; Variables

(defgroup dl-org-flashcards nil
  "Custom variables and library for org flashcards."
  :group 'dl-org)

(defcustom dl-org-flashcards-folder (expand-file-name "flashcards/"
                                                      dl-org-pkm-dir)
  "Dl-org-flashcards file."
  :type 'file
  :group 'dl-org-flashcards)

(defconst dl-org-flashcards-capture-template `("f" "Flashcard" entry
                                               (function dl/org-flashcards-capture-target)
                                               (function dl/org-flashcards-capture-template)
                                               :unnarrowed t)
  "Dl-org-flashcards capture template")

;;; Library

(defun dl/org-flashcards-capture-target ()
  "Return a target for a flashcard capture."
  (when-let ((path (org-roam-node-file (org-capture-get :flashcard-deck))))
    (set-buffer (org-capture-target-buffer path))))

(defun dl/org-flashcards-capture-template ()
  "Return a template for flashcard capture."
  (let* ((node (dl/org-roam-node-read-tag "flashcards"))
         (deck (org-roam-node-title node))
         (type (completing-read "Choose a note type: " '("Basic" "Cloze")))
         (head (concat "* %^{Heading}\n"
                       ":PROPERTIES:\n"
                       (format ":ANKI_NOTE_TYPE: %s\n" type)
                       (format ":ANKI_DECK: %s\n" (capitalize deck))
                       ":END:\n"))
         (body (cond ((string-equal type "Basic") (concat "** Front\n"
                                                          "%?\n"
                                                          "** Back\n\n"))
                     ((string-equal type "Cloze") (concat "** Text\n\n"
                                                          "** Extra\n")))))
    (org-capture-put :flashcard-deck node)
    (concat head body)))

(provide 'dl-org-flashcards)
;;; dl-org-flashcards.el ends here
