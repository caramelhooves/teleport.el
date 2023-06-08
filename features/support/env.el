(require 'f)

(defvar emacs-teleport-support-path
  (f-dirname load-file-name))

(defvar emacs-teleport-features-path
  (f-parent emacs-teleport-support-path))

(defvar emacs-teleport-root-path
  (f-parent emacs-teleport-features-path))

(defvar emacs-teleport-mockup-tsh-path nil)

(add-to-list 'load-path emacs-teleport-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'teleport)
  (require 'espuds)
  (require 'ert))

(defun setup-mock-tsh ()
  (let ((tmp-directory (make-temp-file "emacs-teleport" "directory")))
    (copy-file (format "%s/mock_tsh" emacs-teleport-features-path)
               (format "%s/tsh" tmp-directory)
               nil nil nil "PRESERVE-PERMISSIONS")
    (setq exec-path (cons tmp-directory exec-path))
    (setq emacs-teleport-mockup-tsh-path tmp-directory)))

(defun teardown-mock-tsh ()
  (delete-directory emacs-teleport-mockup-tsh-path t t)
  (setq emacs-teleport-mockup-tsh-path nil))


(Given "^tsh ls prints content of \"\\(.+\\)\"$"
  "Configure tsh mockup to return content of the file"
  (lambda (filename)
    (setenv "EMACS_TELEPORT_TSH_LS_RESPONSE_FILE" (concat emacs-teleport-root-path "/testdata/" filename)))
    )

(Given "^tsh status prints content of \"\\(.+\\)\"$"
  "Configure tsh mockup to return content of the file"
  (lambda (filename)
    (setenv "EMACS_TELEPORT_TSH_STATUS_RESPONSE_FILE" (concat emacs-teleport-root-path "/testdata/" filename)))
    )


(Then "^the buffer should contain content of \"\\([^\"]+\\)\"$"
  (lambda (filename)
    (let ((expected (with-temp-buffer
               (insert-file-contents (concat emacs-teleport-root-path "/testdata/" filename))
               (buffer-string)))
          (actual (buffer-string)))
    (cl-assert (string= expected actual) nil "Expected '%s' to be equal to '%s'" actual expected))))

(Setup
 (setup-mock-tsh)
)

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 (teardown-mock-tsh)
 )
