;;; config-bib.el -*- lexical-binding: t; -*-

(setup bibtex
  (:option bibtex-dialect 'biblatex
           bibtex-auto-key-edit-before-use t
           bibtex-maintain-sorted-entries 'crossref
           bibtex-autokey-name-case-convert-function #'capitalize
           bibtex-autokey-titleword-case-convert-function #'capitalize
           bibtex-autokey-titleword-length t
           bibtex-autokey-titlewords 5
           bibtex-autokey-year-title-separator "-"
           bibtex-autokey-titleword-separator ""
           bibtex-autokey-year-length 4))

(setup (:elpaca ebib)
  (:require dl-bibliography ebib-utils)
  (:option ebib-default-directory dl-bibliography-dir
           ebib-preload-bib-files '(,dl-bibliography-dir)
           ebib-bibtex-dialect 'biblatex
           ebib-use-timestamp t
           ebib-citations-insert-multiple t
           ebib-notes-display-max-lines 10)
  (:add-to-list ebib-file-search-dirs (list (expand-file-name "books" dl-library-dir)
                                            (expand-file-name "papers" dl-library-dir)
                                            (expand-file-name "articles" dl-library-dir))))

(provide 'config-bib)
;;; config-bib.el ends here
