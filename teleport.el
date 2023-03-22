;; teleport.el --- tramp backend for teleport (goteleport.com) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Caramel Hooves
;;
;; Author: Caramel Hooves <caramel.hooves@protonmail.com>
;; Maintainer: Caramel Hooves <caramel.hooves@protonmail.com>
;; Created: March 18, 2023
;; Modified: March 19, 2023
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/caramelhooves/teleport.el
;; Package-Requires: ((emacs "27.1") (vterm "0.0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'tramp)
(require 'dired)
(require 'vterm)

(defcustom teleport-list-nodes-fields nil
"Columns to display in the teleport node list buffer, nil means all"
:group 'teleport
:type '(repeat string))

(defcustom teleport-list-nodes-fields-width 20
"Default width of each column in the teleport list buffer"
:group 'teleport
:type 'integer)

(defcustom teleport-list-nodes-buffer-name "*Teleport Nodes List*"
  "Name of the teleport node list buffer"
  :group 'teleport
  :type 'string)

(defcustom teleport-login-prompts '(("If browser window does not open automatically, open it by clicking on the link.*" . teleport--display-current-buffer)
                                    (".*Password: " . teleport--read-password))
  "Alist of patterns and functions to handle them. If the pattern
is match in stderr output of tsh, the matching function is
called"
  :group 'teleport
  :type '(alist :key-type regex :value-type function))

(defconst teleport-tramp-method "tsh"
  "Tramp method for teleport.")

(defun teleport-tramp-add-method ()
  "Add teleport tramp method."
  (add-to-list 'tramp-methods `("tsh"
                                (tramp-login-program "tsh")
                                (tramp-direct-async t)
                                (tramp-login-args
                                 (("ssh")
                                  ("-l" "%u")
                                  ("%h")))
                                (tramp-copy-args (("scp" "-r")))
                                (tramp-copy-recursive t)
                                (tramp-remote-shell       "/bin/sh")
                                (tramp-remote-shell-args  ("-i" "-c")))))

(defun teleport--tsh-sentinel (process event)
  "Sentinel for caching tsh commands output process. Stores JSON
parsed output in `(process-set process :output-json-symbol)'"
  (with-current-buffer (process-buffer process)
    (cond
     ((string= event "finished\n")
        (goto-char (point-min))
        (let ( ;; A symbol to store the cache
              (output-symbol (process-get process :output-json-symbol))
              ;; function to call to update the buffer
              (completion-notification (process-get process :completion-notification)))

          (set output-symbol (json-parse-buffer))
        (kill-buffer (current-buffer))
        (funcall completion-notification))
        )
   (t (message "Teleport status process failed: %s, %s" event (buffer-string))))))

(defun teleport--display-current-buffer (&rest _)
  "Show current buffer in a new window"
  (display-buffer (current-buffer)))

(defun teleport--read-password (tsh-process)
  "Read password from minibuffer"
  (process-send-string tsh-process (format "%s\n" (read-passwd (buffer-string)))))

(defun teleport--tsh-stderr-filter (process output)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (dolist (prompt teleport-login-prompts)
      (when (re-search-forward (car prompt) nil t)
        (funcall (cdr prompt) (process-get process :main-process))))))

(defun teleport--tsh-stderr-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (cond
     ((string= event "finished\n")
      (kill-buffer (current-buffer)))
     (t (message "Teleport stderr helper process failed: %s, %s" event (buffer-string))))))

(defun teleport--tsh-cmd-async (output-json-symbol process-symbol completion-notification &rest cmd)
  "Start 'cmd' asynchronously and stores the output in `output-symbol'."
  (let ((output-buffer (generate-new-buffer "*tsh-cmd-async*" t))
        (stderr-process (make-pipe-process :name "tsh-cmd-async-stderr-process"
                                          :buffer (generate-new-buffer "*tsh-cmd-async-stderr*")
                                          :filter #'teleport--tsh-stderr-filter
                                          :sentinel #'teleport--tsh-stderr-sentinel
                                          )))
    (let ((process
           (make-process :name (symbol-name process-symbol)
                         :buffer output-buffer
                         :command cmd
                         :sentinel #'teleport--tsh-sentinel
                         :connection-type 'pipe
                         :stderr stderr-process
                         :noquery t)))
      (process-put process :output-json-symbol output-json-symbol)
      (process-put process :completion-notification completion-notification)
      (process-put stderr-process :main-process process)
      (set process-symbol process))))

(defun teleport--tsh-cmd-async-cached (output-json-symbol process-symbol completion-notification &rest cmd)
  "Start 'cmd' asynchronously if it is not running"

  (if (not (boundp process-symbol))
      (set process-symbol nil))

  (unless completion-notification
    (setq completion-notification (lambda () (message "Teleport cmd done: %s" cmd))))

  (let ((old-proc (symbol-value process-symbol)))
    ;; If the process is *not* running, start the async command
    (unless (and old-proc (process-live-p old-proc))
        (apply #'teleport--tsh-cmd-async output-json-symbol process-symbol completion-notification cmd)))
  (symbol-value output-json-symbol))

(defvar teleport--nodes-async-cache []
  "Cached vector of teleport nodes")

(defvar teleport--nodes-async-process nil
  "Background process for async completion")

(defun teleport--get-nodes-async-cached (&optional completion-notification)
  "Returns cached list of teleport hosts"
  (teleport--tsh-cmd-async-cached 'teleport--nodes-async-cache
                                  'teleport--nodes-async-process
                                  completion-notification
                                  "tsh" "ls" "-f" "json"))

(defvar teleport--logins-async-cache '(nil)
  "Cached hashtable of available teleport hosts")

(defvar teleport--logins-async-process nil
  "Background process for async completion")

(defun teleport--status-completion-async-cached (&optional completion-notification)
  "Returns cached list of logins avaialable"
  (teleport--tsh-cmd-async-cached 'teleport--logins-async-cache
                                  'teleport--logins-async-process
                                  completion-notification
                                  "tsh" "status" "-f" "json"))

(defconst teleport--internal-join-login "-teleport-internal-join"
  "Teleport login used to observe an existing session, it is
  filtered out from the list of available logins")

(defun teleport--get-logins (status)
  "Returns a list of logins from the status output"
  (when status
      (let ((logins (gethash "logins" (gethash "active" status))))
        (seq-remove (lambda (login) (string= teleport--internal-join-login login)) logins))))

(defun teleport-tramp-completion (&optional _)
  "Return list of tramp completion. The function runs
asynchronously and returns cached results."
  (let* ((hosts (teleport--get-hosts-async-cached))
         (status (teleport--status-completion-async-cached))
         (logins (teleport--get-logins status)))
    (cl-loop
        for host across hosts
        for host-name = (gethash "name" (gethash "metadata" host))
        append (mapcar (lambda (login) (list login host-name)) logins))))

(with-eval-after-load 'tramp
  (teleport-tramp-add-method)
  (tramp-set-completion-function teleport-tramp-method '((teleport-tramp-completion ""))))

(defun teleport--completion-annotation-all-fields (metadata &optional prefix)
  "Format an annotation string by printing all fields in METADATA."
  ;(format " cmd_labels: %s" cmd_labels))
  (cl-loop
   for key being the hash-key of metadata
   for value = (gethash key metadata)
   if (hash-table-p value)
   concat (teleport--completion-annotation-all-fields value (concat prefix key "."))
   else
   concat (format "%s%s: %s, " prefix key value)))

(defun teleport--completion-format-field (fieldnames metadata)
  "Format a field from METADATA. if the field is nil, return an empty string"
  (let ((val (--reduce-from (gethash it acc) metadata fieldnames)))
        (if (or (null val) (string= val ""))
            ""
                (format "%s: %s" (string-join fieldnames ".") val))))


(defun teleport--completion-annotation-minimal (metadata &rest _)
  "Format an annotation string, print only the most important fields."
  (let* ((fields (list
                 (teleport--completion-format-field '("addr") metadata)
                (teleport--completion-format-field '("hostname") metadata)
                ))
        (cmd_labels_metadata (gethash "cmd_labels" metadata))
        (cmd_labels
         (--map (format "%s: %s" it (gethash "result" (gethash it cmd_labels_metadata))) (hash-table-keys cmd_labels_metadata))))
    (string-join (-remove #'string-empty-p (nconc fields cmd_labels)) ", ")))

(defun teleport-list-hosts--column-name ()
  (get-text-property (point) 'tabulated-list-column-name))

(defun teleport-list-nodes-mode--kill-column ()
  ""
  (interactive)
  (setq tabulated-list-format (cl-delete (teleport-list-hosts--column-name) tabulated-list-format :key #'car :test #'string= :count 1))
  (setq tabulated-list-entries
                  (teleport-list--hosts-mode-entries teleport--nodes-async-cache (mapcar #'car tabulated-list-format)))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  )

(defun teleport-list-nodes-mode--open-dired ()
  (interactive)
  (dired (format "/%s:root@%s:" teleport-tramp-method (tabulated-list-get-id)))
  )

(defun teleport-list-nodes-mode--open-vterm (&optional arg)
  (interactive "P")
  """ Open a vterm in the current host. ARG has the same meaning as in `vterm' """
  (let ((default-directory (format "/%s:root@%s:" teleport-tramp-method (tabulated-list-get-id))))
    (vterm arg)))


(defun teleport-mode--filter-by-pattern (pattern)
  (interactive (list (read-regexp "Filter by regexp")))
  (let* ((col-name (teleport-list-hosts--column-name))
         (col-index (cl-position col-name tabulated-list-format :key #'car :test #'string=)))
    (setq tabulated-list-entries (cl-remove-if-not (lambda (x) (string-match pattern x)) tabulated-list-entries :key (lambda (x) (elt (cadr x) col-index)))))
    (tabulated-list-print t))

(defun teleport-list--calculate-list-format (hosts)
  (let ((list-format teleport-list-fields))
      (when (not list-format)
        (cl-loop
         for host across hosts
         for spec = (gethash "spec" host)
         for cmd_labels = (hash-table-keys (gethash "cmd_labels" spec))
         do (setq list-format (cl-union list-format cmd_labels :test #'string=))))
      (apply #'vector (mapcar (lambda (name) (list name teleport-list-fields-width nil)) list-format))))

(defun teleport-list--refresh-buffer ()
  (with-current-buffer (get-buffer-create teleport-list-buffer-name)
    (when (seq-empty-p tabulated-list-format)
      (setq tabulated-list-format (teleport-list--calculate-list-format teleport--nodes-async-cache))
      (tabulated-list-init-header))

      (setq tabulated-list-entries (teleport-list--hosts-mode-entries teleport--nodes-async-cache (mapcar #'car tabulated-list-format)))

      (tabulated-list-print t)
      (teleport-list--update-modeline)))

(defun teleport-list-nodes-mode--refresh ()
  "Refresh the list of teleport nodes"
  (interactive)
  (teleport--get-hosts-async-cached #'teleport-list--refresh-buffer)
  (teleport-list--update-modeline))

(defun teleport-list-nodes-mode--reset-columns-and-refresh()
  "Restore the columns to the default (teleport-list-nodes-fields) value and refreshes the list of teleport nodes"
  (interactive)
  (setq tabulated-list-format nil)
  (teleport-list-nodes-mode--refresh))

(defvar teleport-list-nodes-mode-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map tabulated-list-mode-map)
          (define-key map "k" 'teleport-list-nodes-mode--kill-column)
          (define-key map "t" 'teleport-list-nodes-mode--open-vterm)
          (define-key map "d" 'teleport-list-nodes-mode--open-dired)
          (define-key map "g" 'teleport-list-nodes-mode--refresh)
          (define-key map "G" 'teleport-list-nodes-mode--reset-columns-and-refresh)
          (define-key map (kbd "RET") 'teleport-list-nodes-mode--open-dired)
          (define-key map (kbd "/ p") 'teleport-mode--filter-by-pattern)
        map))

(defun teleport-list--hosts-mode-entries (hosts list-format)
  (cl-loop
   with empty-entry = (make-hash-table)
   for host across hosts
   for name = (gethash "name" (gethash "metadata" host))
   for spec = (gethash "spec" host)
   for cmd_labels = (gethash "cmd_labels" spec)
   collect (list name (apply #'vector (mapcar (lambda (name) (gethash "result" (gethash name cmd_labels empty-entry) "")) list-format))))
  )

(defun teleport-list--update-modeline()
  (setq mode-line-process (format "%s%s" (if (process-live-p teleport--nodes-async-process) ":Hosts" "")
                                  (if (process-live-p teleport--logins-async-cache)":Logins" "")))
  (force-mode-line-update t))

(define-derived-mode teleport-list-nodes-mode tabulated-list-mode "Teleport Nodes"
  "Major mode for listing the available teleport hosts.
\\<teleport-mode-map>
\\{teleport-mode-map}"
  :group 'teleport

  ;; TODO
  ;(add-hook 'tabulated-list-revert-hook #'teleport-list-hosts--refresh nil t)
  (teleport-list--refresh-buffer)
  (teleport--get-hosts-async-cached #'teleport-list--refresh-buffer))

(defun teleport-list-nodes ()
  "List all teleport nodes."
  (interactive)
  (switch-to-buffer teleport-list-buffer-name)
  (teleport-list-nodes-mode))

(provide 'teleport)
;;; teleport.el ends here
