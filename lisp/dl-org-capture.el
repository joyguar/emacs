;;; dl-org-capture.el --- dl-org-capture capture functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dl-org)

;;; Variables

(defconst dl-org-capture-inbox-template `("i" "Inbox" entry
                                          (file dl-org-inbox-file)
                                          ,(concat "* TODO %?\n"
                                                   "CREATED: %U")
                                          :clock-in t
                                          :clock-resume t
                                          :kill-buffer t
                                          :unnarrowed t)
  "Dl-Org capture inbox template."
  )

(defconst dl-org-capture-slipbox-template `("s" "Slipbox" entry
                                            (file dl-org-slipbox-file)
                                            "* %?\n")
  "Dl-Org capture slipbox template."
  )

(provide 'dl-org-capture)
;;; dl-org-capture.el ends here
