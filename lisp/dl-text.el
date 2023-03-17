;;; dl-text.el -*- lexical-binding: t; -*-

(defun dl/smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace chracter on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace chracter and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun dl/backward-delete-line ()
  "Backwards delete line from point till beginning."
  (interactive)
  (delete-region (point)
                 (line-beginning-position)))

(defun dl/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing))
        (if bounds
            (kill-region (car bounds) (cdr bounds))
          (error "No %s at point" thing)))))

(defun dl/kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (dl/kill-thing-at-point 'word))

(require 'dl-text)
;;; dl-text.el ends here
