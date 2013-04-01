;; BSD license
;; Author: Marcin Koziej
;; marcin at lolownia dot org
;; website: https://github.com/lolownia/org-pomodoro

(require 'timer)
(require 'org)
(require 'org-timer)
(require 'alert)

(defgroup org-pomodoro nil "Org pomodoro customization"
  :tag "Org Pomodoro"
  :group 'org-progress)

(defvar org-pomodoro-timer nil)
(defvar org-pomodoro-timer-start 0)
(defvar org-pomodoro-phase :none)

(defvar org-pomodoro-length-minutes 25)
(defvar org-pomodoro-break-length-minutes 5)

(defvar org-pomodoro-current-task-heading "Current Task")
(defvar org-pomodoro-todo-today-heading "To Do Today")
(defvar org-pomodoro-activity-inventory-heading "Activity Inventory")

(defvar org-pomodoro-mode-line "")
(put 'org-pomodoro-mode-line 'risky-local-variable t)

(defface org-pomodoro-mode-line
  '((t (:foreground "tomato1")))
  "Org Pomodoro mode line color"
  :group 'faces)

(defconst org-pomodoro-estimated "estimated-pomodoros")
(defconst org-pomodoro-completed "completed-pomodoros")

(defun org-pomodoro-set-mode-line (enable?)
  (or global-mode-string (setq global-mode-string '("")))
  (if enable?
      (when (not (memq 'org-pomodoro-mode-line global-mode-string))
	(setq global-mode-string 
	      (append global-mode-string '(org-pomodoro-mode-line))))
      (setq global-mode-string (delq 'org-pomodoro-mode-line global-mode-string)))
  (force-mode-line-update))

(defun org-pomodoro-seconds-elapsed ()
  (round (- (org-float-time (current-time))
       (org-float-time org-pomodoro-timer-start))))

(defun org-pomodoro-minutes-remaining-text (period)
  (let ((hms (org-timer-secs-to-hms (- period (org-pomodoro-seconds-elapsed)))))
    (substring hms (- (length hms) 5))))

(defun org-pomodoro-update-mode-line ()
  (setq org-pomodoro-mode-line
	(cond 
	 ((eq org-pomodoro-phase :none) "")
	 ((eq org-pomodoro-phase :pomodoro)  
	  (propertize 
	   (format "(%s)" (org-pomodoro-minutes-remaining-text (* 60 org-pomodoro-length-minutes))) 
	   'face 'org-pomodoro-mode-line) )
	 ((eq org-pomodoro-phase :break) 
	  (propertize (format "[break %s]" (org-pomodoro-minutes-remaining-text (* 60 org-pomodoro-break-length-minutes)))
		      'face 'org-pomodoro-update-mode-line))))
  (force-mode-line-update))

(defun org-pomodoro-kill ()
  (mo-log "Kill Pomodoro" 'debug)
  (cancel-timer org-pomodoro-timer)
  (org-pomodoro-set-mode-line nil)
  (setq org-pomodoro-phase :none))

(defun org-pomodoro-heartbeat ()
  (cond
   ((and (eq org-pomodoro-phase :none) org-pomodoro-timer) 
    (cancel-timer org-pomodoro-timer))
   ((eq org-pomodoro-phase :pomodoro)
    (progn
      (mo-log (format "%s %s" org-pomodoro-phase (org-pomodoro-seconds-elapsed)) 'debug)
      (when (> (org-pomodoro-seconds-elapsed) (* 60 org-pomodoro-length-minutes))
        (alert-message-notify "Pomodoro completed!" "Time for a break!"))
	(org-pomodoro-start :break)
	(run-hooks 'org-pomodoro-done-hook))
      (org-pomodoro-update-mode-line)))
   ((eq org-pomodoro-phase :break)
    (progn 
      (when (> (org-pomodoro-seconds-elapsed) (* 60 org-pomodoro-break-length-minutes))
        (alert-message-notify "Break is over" "Ready for another one?")
	(progn 
	  (org-pomodoro-kill)
	  (message "You've smashed the pomodoro")))
      (org-pomodoro-update-mode-line))))

(defun org-pomodoro-start (what)
  (mo-log "Timer starting." 'debug)
  (when org-pomodoro-timer 
    (cancel-timer org-pomodoro-timer))
  (org-pomodoro-set-mode-line t)
  (setq org-pomodoro-phase what
	org-pomodoro-timer-start (current-time)
	org-pomodoro-timer (run-with-timer 1 1 'org-pomodoro-heartbeat)))

(defun org-pomodoro-set-estimate (estimated-pomodoros)
  (interactive "nNumber of pomodoros? ")
  (org-set-property org-pomodoro-estimated (number-to-string estimated-pomodoros))
  (org-set-property org-pomodoro-completed "0"))

(defun org-pomodoro-get-estimate ()
  (org-read-property-value org-pomodoro-estimated))

(defun org-pomodoro-delete-estimate ()
  (org-delete-property org-pomodoro-estimated)
  (org-delete-property org-pomodoro-completed))

(defun org-pomodoro-refile-current ()
  (interactive)
  (org-pomodoro-refile org-pomodoro-current-task-heading))

(defun org-pomodoro-refile-todo-today ()
  (interactive)
  (org-pomodoro-refile org-pomodoro-todo-today-heading))

(defun org-pomodoro-refile-activity-inventory ()
  (interactive)
  (org-pomodoro-refile org-pomodoro-activity-inventory-heading))

(defun org-pomodoro-refile (refile-heading)
  "Move the heading at point to the heading specified."
  (save-excursion
    (beginning-of-line)
    (let ((copy-points (list (point))))
      (re-search-forward "*\*") ;; skip headline of current heading
      (re-search-forward "*")   ;; find the headline of the next heading
      (re-search-backward "\n") ;; back to previous line
      (setq copy-points (append copy-points (point)))
      (goto-char (point-min))
      (if (re-search-forward refile-heading nil t)
          (let* ((start (car copy-points))
                 (end (cdr copy-points))
                 (subtree (buffer-substring-no-properties start end)))
            (delete-region start (1+ end)) ;; include trailing newline
            (newline-and-indent)
            (insert subtree))))))

(defun org-pomodoro-find-latest-date ()
  "Move point to the latest date heading."
  (interactive)
  (let ((date-heading-regex "*** \\[.\\{4\\}-.\\{2\\}-.\\{2\\} .\\{3\\}\\]"))
    (goto-char 1)
    (re-search-forward date-heading-regex)))

(defun org-pomodoro-current-date-regex (current-time)
  (format-time-string "\\[%Y-%m-%d %a\\]" current-time))

(defvar org-pomodoro-done-hook nil)

(add-hook 'org-pomodoro-done-hook
	  '(lambda () 
	     (call-interactively 'org-clock-out)))

(defun org-pomodoro (&optional phase)
  (interactive "p")
  (if (equal org-pomodoro-phase :none)
      (progn
	(if (eq major-mode 'org-mode)
	    (call-interactively 'org-clock-in)
	  (let ((current-prefix-arg '(4)))
	    (call-interactively 'org-clock-in)))
	(org-pomodoro-start :pomodoro))
    (if (y-or-n-p "You are already doing a pomodoro. Would You like to stop it?")
	(org-pomodoro-kill)
      (message "Alright, keep up the good work!"))))

(provide 'org-pomodoro)
