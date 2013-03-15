(load "org-pomodoro")

(ert-deftest org-pomodoro-current-date-regex ()
  (let* ((current-time (date-to-time "Fri, 08-Mar-2013 22:08:00 GMT"))
         (current-time-regex (org-pomodoro-current-date-regex current-time)))
    (should (string-equal "\\[2013-03-08 Fri\\]" current-time-regex))))

(ert-deftest org-pomodoro-find-latest-date ()
  (with-temp-buffer
    (insert-file "test.org")
    (goto-char (point-min))
    (org-pomodoro-find-latest-date)
    (should (= 114 (point)))
    (goto-char (point-max))
    (org-pomodoro-find-latest-date)
    (should (= 114 (point)))))

(ert-deftest org-pomodoro-refile-down ()
  (with-temp-buffer
    (insert-file "test.org")
    (search-forward "test 2")
    (beginning-of-line)
    (forward-char 3)
    (org-pomodoro-refile "Activity Inventory")
    (search-forward "test 2")
    (mo-log (buffer-substring (point-min) (point-max)))
    (should (= 83 (point)))))

(ert-deftest org-pomodoro-refile-up ()
  (with-temp-buffer
    (insert-file "test.org")
    (search-forward "test 2")
    (org-pomodoro-refile "Current Task")
    (goto-char (point-min))
    (search-forward "test 2")
    (should (= 25 (point)))))

(ert-deftest org-pomodoro-refile-nonexistent-heading ()
  (with-temp-buffer
    (insert-file "test.org")
    (search-forward "test 1")
    (org-pomodoro-refile "Non existent")
    (should (= 25 (point)))))
