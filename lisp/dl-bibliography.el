;;; dl-bibliography.el --- org functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dl)

(defgroup dl-bibliography nil
  "Custom variables and library for bibliography."
  :group 'bibtex)

(defcustom dl-bibliography-dir (expand-file-name "bib" dl-user-home-dir)
  "Dl-bibliographies folder."
  :type 'directory
  :group 'dl-bibliography)

(defcustom dl-library-dir (expand-file-name "library" dl-user-sync-dir)
  "Dl-bibliographies folder."
  :type 'directory
  :group 'dl-bibliography)

(provide 'dl-bibliography)
;;; dl-bibliography.el ends here
