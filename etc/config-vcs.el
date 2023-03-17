;;; config-vcs.el -*- lexical-binding: t; -*-

(setup (:elpaca magit)
  (:option magit-git-executable (executable-find "git")))

(provide 'config-vcs)
;;; config-vcs.el ends here
