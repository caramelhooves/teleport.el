;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^I wait for \\([0-9]+\\) seconds$"
      (lambda (seconds)
        (sleep-for (string-to-number seconds))))

(Then "^Buffer \"\\([^\"]+\\)\" contains \"\\([^\"]+\\)\"$"
      (lambda (buffer-name text)
        (with-current-buffer buffer-name
          (print (buffer-string))
          (cl-assert (string-search text (buffer-string))))))
