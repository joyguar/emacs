;;; dl-setup.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'setup)
(require 'elpaca)
(require 'dl)

;; (defun dl/setup-warn (message &rest args)
;;   "Warn the user that something bad happened in `setup'."
;;   (display-warning 'setup (format message args)))

;; (defun dl/setup-wrap-to-demote-errors (body name)
;;   "Wrap BODY in a `with-demoted-errors' block.
;; This behavior is prevented if `setup-attributes' contains the
;; symbol `without-error-demotion'.

;; This function differs from `setup-wrap-to-demote-errors' in that
;; it includes the NAME of the setup form in the warning output."
;;   (if (memq 'without-error-demotion setup-attributes)
;;       body
;;     `(with-demoted-errors ,(format "Error in setup form on line %d (%s): %%S"
;;                                    (line-number-at-pos)
;;                                    name)
;;        ,body)))

(defun dl/setup-wrap-to-install-package (body _name)
  "Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
  (if (assq 'elpaca setup-attributes)
      `(elpaca ,(cdr (assq 'elpaca setup-attributes))
	 ,(macroexp-progn (cdr (macroexp-unprogn body))))
    body))

;; New forms

(setup-define :elpaca
  (lambda (order)
    (push (cond (`(elpaca . ,order)))
          setup-attributes))
  :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe)
                     (car recipe)
                   recipe))))

(setup-define :autoload
  (lambda (func)
    (let ((fun (if (memq (car-safe func) '(quote function))
                   (cadr func)
                 func)))
          `(unless (fboundp (quote ,fn))
             (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
               :documetnation "Autoload COMMAND if not already bound."
               :repeatable t
               :signature '(FUNC ...))

(setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :load-from
    (lambda (path)
      `(let ((path* (expand-file-name ,path)))
         (if (file-exists-p path*)
             (add-to-list 'load-path path*)
           ,(setup-quit))))
  :documentation "Add PATH to load path.
This macro can be used as NAME, and it will replace itself with
the nondirectory part of PATH.
If PATH does not exist, abort the evaluation."
  :shorthand (lambda (args)
               (intern
                (file-name-nondirectory
                 (directory-file-name (cadr args))))))

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable)
       ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :opt
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
       ',name))
   (lambda (name val)
     `(funcall (or (get ',name 'custom-set)
                   #'set-default)
       ',name ,val)))
  :documentation "Set the option NAME to VAL.
This omits the loading of all dependencies of the named symbols."
  :debug '(sexp form)
  :repeatable t)

(setup-define :face
    (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
    :documentation "Customize FACE to SPEC."
    :signature '(face spec ...)
    :debug '(setup)
    :repeatable t
    :after-loaded t)

;; (setup-define :option
;;    (lambda (name val)
;;      '(progn
;;        (custom-load-symbol ',name)
;;        (funcall (or (get ',name 'custom-set)
;;                     #'set-default)
;;                 ',name ,val)))
;;   :documentation "Set the option NAME to VAL.
;; This omits the loading of all dependencies of the named symbols."
;;   :debug '(sexp form)
;;   :repeatable t)

(setup-define :option-after
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
               ',name))
   (lambda (name val)
     `(progn
        (custom-load-symbol ',name)
        (funcall (or (get ',name 'custom-set) #'set-default)
                 ',name ,val))))
  :documentation "Set the option NAME to VAL.
NAME may be a symbol, or a cons-cell.  If NAME is a cons-cell, it
will use the car value to modify the behaviour.  These forms are
supported:

(append VAR)    Assuming VAR designates a list, add VAL as its last
                element, unless it is already member of the list.

(prepend VAR)   Assuming VAR designates a list, add VAL to the
                beginning, unless it is already member of the
                list.

(remove VAR)    Assuming VAR designates a list, remove all instances
                of VAL.

(append* VAR)  Assuming VAR designates a list, add each element
               of VAL to the end of VAR, keeping their order,
               unless it is already a member of the list.

(prepend* VAR) Assuming VAR designates a list, add each element
               of VAL to the start of VAR, keeping their order,
               unless it is already a member of the list.

(remove* VAR)  Assuming VAR designates a list, remove all
               instances of each element of VAL.

Note that if the value of an option is modified partially by
append, prepend, remove, one should ensure that the default value
has been loaded. Also keep in mind that user options customized
with this macro are not added to the \"user\" theme, and will
therefore not be stored in `custom-set-variables' blocks."
  :debug '(sexp form)
  :repeatable t
  :after-loaded t)

(setup-define :advise
  (lambda (symbol where function)
    `(advice-add ,symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

(setup-define :long-hook
  (lambda (hook function)
    `(add-hook ',hook ,function))
  :documentation "Add FUNCTION to HOOK in context-less longhand."
  :ensure '(nil func)
  :repeatable t)

(setup-define :add-to-list
  (lambda (list-var to-add)
    `(if (symbolp ,to-add)
         (add-to-list ',list-var ,to-add)
       (dolist (item ,to-add)
         (add-to-list ',list-var item))))
  :documentation "Add TO-ADD to LIST-VAR.
If TO-ADD is a list, add items from TO-ADD to LIST-VAR piecewise."
  :repeatable t)

(setup-define :enable-mode
  (lambda (name)
    `(setopt ,name t))
  :documentation "Enable MODE."
  :repeatable t)

(setup-define :disable-mode
  (lambda (name)
    `(setopt ,name nil))
  :documentation "Disable MODE."
  :repeatable t)

(provide 'dl-setup)
;;; dl-setup.el ends here
