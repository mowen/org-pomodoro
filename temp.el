;; org-insert-heading-after-current will insert a new heading

;; org-insert-todo-heading can insert a TODO

;; org-end-of-line will move to the end of the line, there is no beginning of
;; line equivalent (org-back-to-heading?)

;; org-make-org-heading-search-string will make a search string for a heading or string

(defun org-pomodoro-kill ()
  (cancel-timer org-pomodoro-timer)
  (org-pomodoro-set-mode-line nil)
  (setq org-pomodoro-phase :none))

