;;; config-dirs.el ---  dired mode settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Builtin

(setup dired
  (:require dired-x)
  (:option dired-recursive-copies 'always
            dired-recursive-deletes 'top
            dired-mouse-drag-files t
            dired-listing-switches (concat "-l "
                                           "--almost-all "
                                           "--human-readable "
                                           "--group-directories-first "
                                           "--no-group"))
  (put 'dired-find-alternate-file 'disabled nil))

;;; External

(setup (:elpaca dirvish)
  (:option dirvish-reuse-session nil
           dirvish-cache-dir (dl/emacs-cache "dirvish")
           dirvish-attributes '(all-the-icons
                                file-time
                                file-size
                                collapse
                                subtree-state
                                vs-state
                                git-msg)
           dirvish-preview-dispatchers '(image
                                         gif
                                         video
                                         audio
                                         epub
                                         archive
                                         pdf-preface)
           dirvish-quick-access-entries '(("h" "~/" "home")
                                          ("d" "~/downloads/" "downloads")
                                          ("m" "/mnt/" "drives")
                                          ("t" "~/.local/share/Trash/files/" "trash")
                                          ("s" "~/src/" "sources")
                                          ("c" "~/.config/" "configs"))
           dirvish-emerge-groups '(("Documents" (extensions "pdf" "tex" "bib" "epub"))
                                   ("Video" (extensions "mp4" "mkv" "webm"))
                                   ("Pictures" (extensions "jpg" "png" "svg" "gif"))
                                   ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                                   ("Archives" (extensions "gz" "rar" "zip")))
           dirvish-override-dired-mode t
           dirvish-peek-mode t)
  (:global [remap dired] #'dirvish))

(provide 'config-dirs)
;;; config-dirs.el ends here
