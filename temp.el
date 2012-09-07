;; org-insert-heading-after-current will insert a new heading

;; org-insert-todo-heading can insert a TODO

;; org-end-of-line will move to the end of the line, there is no beginning of
;; line equivalent (org-back-to-heading?)

;; org-make-org-heading-search-string will make a search string for a heading or string

;; New property-based version

(defconst org-pomodoro-estimated "estimated-pomodoros")
(defconst org-pomodoro-completed "completed-pomodoros")

(defun org-pomodoro-kill ()
  (cancel-timer org-pomodoro-timer)
  (org-pomodoro-set-mode-line nil)
  (setq org-pomodoro-phase :none))

(defun org-pomodoro-set-estimate (estimated-pomodoros)
  (interactive "nNumber of pomodoros? ")
  (org-set-property org-pomodoro-estimated (number-to-string estimated-pomodoros))
  (org-set-property org-pomodoro-completed "0"))

(defun org-pomodoro-get-estimate ()
  (org-read-property-value org-pomodoro-estimated))

(defun org-pomodoro-delete-estimate ()
  (org-delete-property org-pomodoro-estimated)
  (org-delete-property org-pomodoro-completed))
