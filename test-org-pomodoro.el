(load "org-pomodoro")

(ert-deftest org-pomodoro-current-date-regex ()
  (let ((current-time (date-to-time "Fri, 08-Mar-2013 22:08:00 GMT")))
    (should (string-equal "\\[2013-03-08 Fri\\]"
                          (org-pomodoro-current-date-regex current-time)))))

(ert-deftest org-pomodoro-find-latest-date ()
  (with-temp-buffer
    (insert-file "test.org")
    (goto-char (point-min))
    (org-pomodoro-find-latest-date)
    (should (= 114 (point)))
    (goto-char (point-max))
    (org-pomodoro-find-latest-date)
    (should (= 114 (point)))))

(ert-deftest org-pomodoro-refile ()
  (with-temp-buffer
    (insert-file "test.org")
    (search-forward "test 2")
    (beginning-of-line)
    (forward-char 3)
    (org-pomodoro-refile "Activity Inventory")
    (search-forward "test 2")
    (should (= 83 (point)))))
