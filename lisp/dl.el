;;; dl.el -*- lexical-binding: t; -*-

(defgroup dl nil
  "Core variables and library for dotemacs."
  :group 'editing)

;; XDG dirs

(defcustom dl-xdg-config-dir (or (getenv-internal "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config"))
  "`freedesktop.org' config folder."
  :type 'directory
  :group 'dl)

(defcustom dl-xdg-cache-dir (or (getenv-internal "XDG_CACHE_HOME")
                               (expand-file-name "~/.cache"))
  "`freedesktop.org' cache folder."
  :type 'directory
  :group 'dl)

(defcustom dl-xdg-data-dir (or (getenv-internal "XDG_DATA_HOME")
                              (expand-file-name "~/.local/share"))
  "`freedesktop.org' data folder."
  :type 'directory
  :group 'dl)

(defcustom dl-xdg-state-dir (or (getenv-internal "XDG_STATE_HOME")
                               (expand-file-name "~/.local/state"))
  "`freedesktop.org' state folder."
  :type 'directory
  :group 'dl)

(defcustom dl-xdg-runtime-dir (or (getenv-internal "XDG_RUNTIME_DIR")
                                 (format "/run/user/%s" (user-real-uid)))
  "`freedesktop.org' runtime folder."
  :type 'directory
  :group 'dl)

;; Emacs dirs

(defcustom dl-emacs-user-dir user-emacs-directory
  "Emacs user folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-config-dir (expand-file-name "etc"
                                                user-emacs-directory)
  "Emacs config folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-lisp-dir (expand-file-name "lisp"
                                              user-emacs-directory)
  "Emacs lisp folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-local-dir (expand-file-name ".local"
                                               user-emacs-directory)
  "Emacs local folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-site-lisp-dir (expand-file-name "site-lisp"
                                                    dl-emacs-local-dir)
  "Emacs site-lisp folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-cache-dir (expand-file-name "cache"
                                               dl-emacs-local-dir)
  "Emacs cache folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-data-dir (expand-file-name "var"
                                               dl-emacs-local-dir)
  "Emacs data folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-state-dir (expand-file-name "state"
                                                dl-emacs-local-dir)
  "Emacs state folder."
  :type 'directory
  :group 'dl)

(defcustom dl-emacs-runtime-dir (expand-file-name "emacs"
                                                  dl-xdg-runtime-dir)
  "Emacs runtime folder."
  :type 'directory
  :group 'dl)

;; Misc Dirs

(defcustom dl-user-home-dir (expand-file-name "~")
  "User's home folder."
  :type 'directory
  :group 'dl)

(defcustom dl-user-sync-dir (expand-file-name "sync"
                                              dl-user-home-dir)
  "User's sync folder."
  :type 'directory
  :group 'dl)

;; Custom Hooks

(defcustom dl-emacs-config-hook nil
  "Normal hook run after Emacs init and command line parsing is done."
  :type 'hook
  :group 'dl)

;; XDG

(defun dl/xdg-config (file)
  "Return FILE expanded with `dl-xdg-config-dir'."
  (expand-file-name file dl-xdg-config-dir))

(defun dl/xdg-cache (file)
  "Return FILE expanded with `dl-xdg-cache-dir'."
  (expand-file-name file dl-xdg-cache-dir))

(defun dl/xdg-data (file)
  "Return FILE expanded with `dl-xdg-data-dir'."
  (expand-file-name file dl-xdg-data-dir))

;; Emacs

(defun dl/emacs-user (file)
  "Return FILE expanded with `dl-emacs-user-dir'."
  (expand-file-name file dl-emacs-user-dir))

(defun dl/emacs-config (file)
  "Return FILE expanded with `dl-emacs-config-dir'."
  (expand-file-name file dl-emacs-config-dir))

(defun dl/emacs-cache (file)
  "Return FILE expanded with `dl-emacs-cache-dir'."
  (expand-file-name file dl-emacs-cache-dir))

(defun dl/emacs-data (file)
  "Return FILE expanded with `dl-emacs-data-dir'."
  (expand-file-name file dl-emacs-data-dir))

(defun dl/emacs-lisp (file)
  "Return FILE expanded with `dl-emacs-lisp-dir'."
  (expand-file-name file dl-emacs-lisp-dir))

(defun dl/emacs-site-lisp (file)
  "Return FILE expanded with `dl-emacs-site-lisp-dir'."
  (expand-file-name file dl-emacs-site-lisp-dir))

(defun dl/emacs-state (file)
  "Return FILE expanded with `dl-emacs-state-dir'."
  (expand-file-name file dl-emacs-state-dir))

(defun dl/emacs-runtime-dir (file)
  "Return FILE expanded with `dl-emacs-runtime-dir'."
  (expand-file-name file dl-emacs-runtime-dir))

;; Public library

(defmacro dl/advices-add (&rest subforms)
  "Create advices with `advice-add' from SUBFORMS."
  (macroexp-progn
   (named-let parse ((subforms subforms))
     (pcase-exhaustive subforms
       ('() '())
       ((or `(,sym ,where ,fn :props ,(app list props) . ,rest)
            (and `(,sym ,where ,fn . ,rest) (let props '())))
        (cons `(advice-add ,sym ,where ,fn ,@props) (parse rest)))))))

(defmacro dl/setopt (&rest settings)
  "Apply each pair of SETTINGS."
  (macroexp-progn
   (cl-loop for (name val) on settings by 'cddr
            collect `(funcall (or (get ',name 'custom-set)
                                  #'set-default-toplevel-value)
                      ',name ,val))))

(defmacro dl/with-progress (pr-args &rest body)
  "Perform BODY wrapped in a progress reporter.
PR-ARGS is the list of arguments to pass to
`make-progress-reporter'; it can be a single string for the
message, as well. If you want to use a formatted string, wrap
the `format' call in a list."
  (declare (indent 1))
  (let ((reporter (gensym))
        (pr-args (if (listp pr-args) pr-args (list pr-args))))
    `(let ((,reporter (make-progress-reporter ,@pr-args)))
       (prog1 (progn ,@body)
         (progress-reporter-done ,reporter)))))

(defun dl/auto-create-missing-dirs ()
  "Automatically create missing directories when finding a file."
  (unless (file-remote-p buffer-file-name)
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (and (not (file-directory-p target-dir))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                                     target-dir)))
        (make-directory target-dir t)))))

(defun dl/add-to-list (list-var to-add &optional append compare-fn)
  "Add TO-ADD to LIST-VAR.
If TO-ADD is a list, add items from TO-ADD to LIST-VAR piecewise."
  (if (listp to-add)
      (dolist (item to-add)
        (add-to-list list-var item append compare-fn))
    (add-to-list list-var to-add append compare-fn)))

(defun dl/apply-for-each-file-in (dir func &optional regexp)
  "Apply for each file name in DIR function FUNC filtered by REGEXP."
  (when (file-exists-p dir)
    (mapc func (sort (directory-files-recursively dir (or regexp "^.+$"))
                     'string<))))

(defun dl/require-dir (dir)
  "Load all packages from DIR."
  (when (file-exists-p dir)
    (dl/apply-for-each-file-in
     dir
     (lambda (fname)
       (let ((feature (intern (file-name-base fname)))
             (path (file-name-directory fname)))
         (add-to-list 'load-path path)
         (require feature)))
     "\\.el$")))

(defun dl/hl-line-range ()
  (save-excursion
      (cons (progn (vertical-motion 0) (point))
            (progn (vertical-motion 1) (+ (point) 0)))))

(defun dl/paren-match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1)))))

(defun dl/delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; Advice

(defun en/disable-command@write-to-data-dir ()
  (let ((user-init-file custom-file))
    (apply fn args)))

(provide 'dl)
;;; dl.el ends here
