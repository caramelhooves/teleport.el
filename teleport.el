;;; teleport.el --- Integration for tsh (goteleport.com) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Caramel Hooves
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; Author: Caramel Hooves <caramel.hooves@protonmail.com>
;; Maintainer: Caramel Hooves <caramel.hooves@protonmail.com>
;; Created: March 18, 2023
;; Modified: March 19, 2023
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/caramelhooves/teleport.el
;; Package-Requires: ((emacs "28.1") (dash "2.18.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Provides a way to list all connected teleport nodes and connect to them via TRAMP.
;;
;;; Code:

(require 'tramp)
(define-widget 'teleport-list-filed-config 'lazy
  "Configuration for a single column in the teleport node list buffer"
  :offset 4
  :tag "Column"
  :type '(group :tag "Properties"
          (const :format "" :value :name)
          (string :tag "Name" :value "")
          (const :format "" :value :path)
          (repeat :tag "Path" (string))
          (set :inline t :tag "Optional properties"
               (list :inline t :format "%v"
                     (const :format "" :value :width)
                     (integer :tag "Custom Width"))
               (const :tag "Hide" :inline t :value (:hidden t)))))

(defcustom teleport-list-nodes-fields nil
  "Columns to display in the teleport node list buffer."
  :group 'teleport
  :type '(repeat teleport-list-filed-config))


(defcustom teleport-list-nodes-fields-width 20
  "Default width of each column in the teleport list buffer."
  :group 'teleport
  :type 'integer)

(defcustom teleport-list-nodes-buffer-name "*Teleport Nodes List*"
  "Name of the teleport node list buffer."
  :group 'teleport
  :type 'string)

(defcustom teleport-login-prompts
  '(("If browser window does not open automatically, open it by clicking on the link.*"
     .
     teleport--display-current-buffer)
    (".*Password: " . teleport--read-password)
    ("Enter password for Teleport user .*" . teleport--read-password)
    ("Enter your OTP token:" . teleport--read-password))
  "Alist of patterns and functions to handle them.
If the pattern
is match in stderr output of tsh, the matching function is called"
  :group 'teleport
  :type '(alist :key-type regex :value-type function))

(defcustom teleport-list-nodes-show-hostname t
  "Show node's hostname in the list."
  :type 'boolean
  :group 'teleport)

(defcustom teleport-list-nodes-hostname-column ".hostname"
  "Column name to display node's hostname.
Should be unique and not
overlap with any existing cmd_labels."
  :type 'string
  :group 'teleport)

(defconst teleport-tramp-method "tsh"
  "Tramp method name for teleport nodes.")

(defcustom teleport-shell-command-buffer-name
  "*Teleport Shell Command Output: %s*"
  "Basename used for teleport shell buffers"
    :group 'teleport
    :type 'string)

(defvar-local teleport-list-nodes--default-directory nil)

(defvar-local teleport-list-nodes--filter-by-pattern '())
;;;###autoload
(defun teleport-tramp-add-method ()
  "Add teleport tramp method."
  (add-to-list
   'tramp-methods
   `(,teleport-tramp-method
     (tramp-login-program "tsh")
     (tramp-direct-async t)
     (tramp-login-args (("ssh") ("-l" "%u") ("%h")))
     (tramp-copy-program "tsh")
     (tramp-copy-args (("scp") ("--preserve")))
     (tramp-copy-keep-date t)
     (tramp-remote-shell ,tramp-default-remote-shell)
     (tramp-remote-shell-args ("-i" "-c"))))
  (tramp-set-completion-function
     teleport-tramp-method '((teleport-tramp-completion ""))))

(defun teleport--tsh-sentinel (process event)
  "Process sentinel for tsh -f json commands.
Parse and store output of PROCESS in `(process-set process :output-json-symbol)'
if EVENT indicates a failure, display an error message with the buffer content."
  ;;TODO handle errors as described in the docs
  ;;(info "elisp#Accepting Output")
  (with-current-buffer (process-buffer process)
    (cond
     ((string= event "finished\n")
      (goto-char (point-min))
      (let ( ;; A symbol to store the cache
            (output-symbol (process-get process :output-json-symbol))
            ;; function to call to update the buffer
            (completion-notification
             (process-get process :completion-notification)))

        (set output-symbol (json-parse-buffer))
        (kill-buffer (current-buffer))
        (funcall completion-notification)))
     (t
      (message "Teleport process failed: %s, %s, stderr: %s"
               event
               (buffer-string)
               (with-current-buffer (process-get process :stderr-buffer) (buffer-string)))
      (teleport-list--update-modeline-from-anywhere)))))

(defun teleport-list--update-modeline-from-anywhere ()
  "Update modeline for teleport buffer."
  (with-current-buffer teleport-list-nodes-buffer-name
        (teleport-list--update-modeline)))

(defun teleport--display-current-buffer (&rest _)
  "Show current buffer in a new window."
  (display-buffer (current-buffer)))

(defun teleport--read-password (tsh-process)
  "Read password from minibuffer and inject it into stdin of TSH-PROCESS."
  (process-send-string
   tsh-process (format "%s\n" (read-passwd (buffer-string)))))

(defun teleport--tsh-stderr-filter (process output)
  "Process filter for OUTPUT of tsh -f json commands.
If PROCESS requires a password, read it from minibuffer and
inject it into stdin."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)
    (dolist (prompt teleport-login-prompts)
      (when (re-search-backward (car prompt) nil t)
        (funcall (cdr prompt) (process-get process :main-process))))))

(defun teleport--tsh-stderr-sentinel (process event)
  "Process sentinel for `make-pipe-process'.
If PROCESS fails, display an error message with the buffer content.
EVENT is the event that caused the process to exit."
  (with-current-buffer (process-buffer process)
    (cond
     ((string= event "finished\n")
      (delete-windows-on (current-buffer) 't)
      (kill-buffer))
     (t
      (message "Teleport stderr helper process failed: %s, %s"
               event
               (buffer-string))))))

(defun teleport--tsh-cmd-async
    (output-json-symbol
     process-symbol completion-notification &rest cmd)
  "Start CMD asynchronously and store the output in OUTPUT-JSON-SYMBOL.
Store process object in PROCESS-SYMBOL. When the process
terminates, call COMPLETION-NOTIFICATION."
  (let ((output-buffer (generate-new-buffer "*tsh-cmd-async*" t))
        (stderr-process
         (make-pipe-process
          :name "tsh-cmd-async-stderr-process"
          :buffer
          (generate-new-buffer "*tsh-cmd-async-stderr*")
          :filter #'teleport--tsh-stderr-filter
          :sentinel #'teleport--tsh-stderr-sentinel)))
    (let ((process
           (make-process
            :name (symbol-name process-symbol)
            :buffer output-buffer
            :command cmd
            :sentinel #'teleport--tsh-sentinel
            :connection-type 'pipe
            :stderr stderr-process
            :noquery t)))
      (process-put process :output-json-symbol output-json-symbol)
      (process-put process :completion-notification completion-notification)
      (process-put process :stderr-buffer (process-buffer stderr-process))
      (process-put stderr-process :main-process process)
      (set process-symbol process))))

(defun teleport--tsh-cmd-async-cached (output-json-symbol process-symbol completion-notification &rest cmd)
  "Start CMD asynchronously if PROCESS-SYMBOL process is not running.
Store the result in OUTPUT-JSON-SYMBOL and call
COMPLETION-NOTIFICATION when done."

  (unless completion-notification
    (setq completion-notification
          (lambda () (message "Teleport cmd done: %s" cmd))))

  (let ((old-proc (symbol-value process-symbol)))
    ;; If the process is *not* running, start the async command
    (unless (process-live-p old-proc)
      (apply #'teleport--tsh-cmd-async
             output-json-symbol
             process-symbol
             completion-notification
             cmd)))
  (symbol-value output-json-symbol))

(defvar teleport--nodes-async-cache []
  "Cached vector of teleport nodes.")

(defvar teleport--nodes-async-process nil
  "Background process for async completion.")

(defun teleport--get-nodes-async-cached
    (&optional completion-notification)
  "Return cached list of teleport nodes.
Call COMPLETION-NOTIFICATION when a new list is available."
  (teleport--tsh-cmd-async-cached 'teleport--nodes-async-cache
                                  'teleport--nodes-async-process
                                  completion-notification
                                  "tsh"
                                  "ls"
                                  "-f"
                                  "json"))

(defvar teleport--logins-async-cache '(nil)
  "Cached hashtable of available teleport nodes.")

(defvar teleport--logins-async-process nil
  "Background process for async completion.")

(defun teleport--status-completion-async-cached
    (&optional completion-notification)
  "Return cached list of logins available.
Call COMPLETION-NOTIFICATION when a new list is available."
  (teleport--tsh-cmd-async-cached 'teleport--logins-async-cache
                                  'teleport--logins-async-process
                                  completion-notification
                                  "tsh"
                                  "status"
                                  "-f"
                                  "json"))

(defconst teleport--internal-join-login "-teleport-internal-join"
  "Teleport login used to observe an existing session.
It is filtered out from the list of available logins")

(defun teleport--get-logins (status)
  "Return a list of logins from the STATUS output."
  (when status
    (let ((logins (gethash "logins" (gethash "active" status))))
      (seq-remove
       (lambda (login)
         (string= teleport--internal-join-login login))
       logins))))

(defun teleport-tramp-completion (&optional _)
  "Return list of tramp completion.
The function runs asynchronously and returns cached results."
  (let* ((nodes (teleport--get-nodes-async-cached))
         (status (teleport--status-completion-async-cached))
         (logins (teleport--get-logins status)))
    (cl-loop
     for host across nodes
     for host-name = (gethash "name" (gethash "metadata" host))
     append (mapcar (lambda (login) (list login host-name)) logins))))


(defun teleport--completion-annotation-all-fields
    (metadata &optional prefix)
  "Format an annotation string by printing all fields in METADATA.
PREFIX is prepended to the field name."
  (cl-loop
   for key being the hash-key of metadata
   for value =(gethash key metadata)
   if (hash-table-p value)
   concat (teleport--completion-annotation-all-fields value
                                               (concat
                                                prefix key "."))
   else
   concat
   (format "%s%s: %s, " prefix key value)))

(defun teleport--seq-last (sequence)
  "Return the last element of SEQUENCE."
  ;; I wonder why it is not implemented in `seq'
  (seq-elt sequence (- (length sequence) 1)))

(defun teleport-list-nodes--column-name ()
  "Return column name at point."
  (or
   (get-text-property (point) 'tabulated-list-column-name)
   (car (teleport--seq-last tabulated-list-format))))

(defun teleport-list-nodes-mode--kill-column ()
  "Hide the column at point."
  (interactive)
  (let ((col (cl-find
   (teleport-list-nodes--column-name)
   teleport-list-nodes-fields
   :key (lambda (x) (plist-get x :name)))))
    (message "Hide column %s" col)
    (plist-put col :hidden t))
  (teleport-list--refresh-buffer))

(defun teleport-mode--filter-by-pattern (column pattern)
  "Add a filter for list of nodes.
Only nodes with COLUMN matching PATTERN will be shown. The filter
could be applied to multiple columns. To remove the filter, use `teleport-mode--reset-filter-by-pattern'."
  (interactive (list (completing-read "Filter in column: "
                                      (mapcar (lambda (x) (plist-get x :name)) teleport-list-nodes-fields)
                                      #'identity
                                      t
                                      (cons (teleport-list-nodes--column-name) 0))
                (read-regexp "Filter by regexp: ")))
  (add-to-list 'teleport-list-nodes--filter-by-pattern (cons column pattern))
  (teleport-list--refresh-buffer))

(defun teleport-mode--apply-filter-by-pattern (list-of-patterns)
  "Remove entries that do not match patterns in LIST-OF-PATTERNS."

  (cl-loop
   for (column . pattern) in list-of-patterns
   for col-index = (cl-position column tabulated-list-format
                       :key #'car
                       :test #'string=)
   do
    (setq tabulated-list-entries
          (cl-remove-if-not
           (lambda (x) (string-match pattern x))
           tabulated-list-entries
           :key (lambda (x) (elt (cadr x) col-index))))))

(defun teleport-mode--reset-filter-by-pattern ()
  "Remove all filters."
  (interactive)
  (setq teleport-list-nodes--filter-by-pattern '())
  (teleport-list--refresh-buffer))

(defun teleport-list-nodes--get-list-of-unique-cmd-labels (nodes)
  "Return a list of unique cmd_labels from NODES."
  (cl-loop
   for node across nodes
   for spec = (gethash "spec" node)
   for cmd_labels = (gethash "cmd_labels" spec)
   when cmd_labels
   for cmd-labels-path = (hash-table-keys cmd_labels) then (cl-union cmd-labels-path (hash-table-keys cmd_labels) :test #'equal)
   finally return (mapcar (lambda (label)
                            `(,label
                              ("cmd_labels" ,label "result")))
                          cmd-labels-path)))

(defun teleport--refresh-available-spec (nodes)
  "Iterate over NODES and collect all unique cmd_labels names.
Stores them in `teleport-list-nodes-fields'"

  ;; Collect all available cmd_labels path from spec. `cmd-labels-path' is a list
  ;; of (label . path), where path is a list of strings showing full path in
  ;; node's JSON
  (let* ((cmd-labels-path (teleport-list-nodes--get-list-of-unique-cmd-labels nodes))
         (all-specs-path
          (if teleport-list-nodes-show-hostname
              ;;FIXME: what if cmd_labels also have 'hostname'? tabulated mode
              ;;does not expect duplicate column names.
              (cons `(,teleport-list-nodes-hostname-column ("hostname")) cmd-labels-path)
            cmd-labels-path)))

      ;; Find which specs are missing from `teleport-list-nodes-fields', compare only 'path' field
      (let ((missing-specs
             (cl-set-difference
              all-specs-path
              (mapcar (lambda (x) (list (plist-get x :name) (plist-get x :path))) teleport-list-nodes-fields)
              :test #'equal
              :key #'cadr)))
      ;; Add them to the `teleport-list-nodes-fields' with default width `teleport-list-nodes-fields-width'
        (setq teleport-list-nodes-fields
              (nconc teleport-list-nodes-fields
                     (mapcar (lambda (x) `(:name ,(car x) :path ,(cadr x)))
                             missing-specs))))))

(defun teleport-list--calculate-list-format (nodes-fields)
  "Return vector suitable for `tabulated-format-list'.
Convert NODES-FIELDS(`teleport-list-nodes-fields') into format
 accepted by `tabulated-mode'. Hidden fields are removed, field
 width is set to `teleport-list-nodes-fields-width' if not
 specified."
  (let ((enabled-fields
         (seq-filter (lambda (x) (not (plist-get x :hidden))) nodes-fields)))
  (apply #'vector (mapcar (lambda (x) (list
                                       (plist-get x :name)
                                       (or (plist-get x :width) teleport-list-nodes-fields-width)
                                       t)) enabled-fields))))

(defun teleport-list--refresh-buffer ()
  "Refresh the buffer with the list of teleport nodes.
Does not fetch the list of nodes. Could be used to un-do effects
of `teleport-mode--filter-by-pattern'."
  (interactive)
  (with-current-buffer (get-buffer-create
                        teleport-list-nodes-buffer-name)
    (teleport--refresh-available-spec teleport--nodes-async-cache)
    (setq tabulated-list-format
          (teleport-list--calculate-list-format
           teleport-list-nodes-fields))

    (setq tabulated-list-entries
          (teleport-list--nodes-mode-entries
           teleport--nodes-async-cache
           teleport-list-nodes-fields))
    (teleport-mode--apply-filter-by-pattern teleport-list-nodes--filter-by-pattern)
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (teleport-list--update-modeline)))

(defun teleport-list-nodes-mode--update-list ()
  "Fetch the list of teleport nodes and refresh the buffer."
  (interactive)
  (teleport--get-nodes-async-cached #'teleport-list--refresh-buffer)
  (teleport-list--update-modeline))

(defun teleport-list-nodes-mode--reset-columns ()
  "Reset state of the columns.
Remove any customization of the displayed columns and re-fill
them with the columns from the currently available nodes."
  (interactive)
  (setq teleport-list-nodes-fields nil)
  (teleport--refresh-available-spec teleport--nodes-async-cache)
  (setq tabulated-list-format
        (teleport-list--calculate-list-format
         teleport-list-nodes-fields))
  (teleport-list--refresh-buffer))

(defun teleport-list-nodes-mode--customize-displayed-fields ()
  "Customize the displayed columns."
  (interactive)
  (customize-variable 'teleport-list-nodes-fields))

(defun teleport-list-nodes--get-ids ()
  "Return a list of tagged IDs.
If no nodes are tagged, return node at point"
  (let (;; Match non-space in the first n characters.
        (re (format "^ \\{0,%d\\}[^ ]" (1- tabulated-list-padding))))
    (let ((tagged-nodes
           (save-excursion (goto-char (point-min))
                           (cl-loop
                            while (re-search-forward re nil 'noerror)
                            collect (tabulated-list-get-id)))))
      (or tagged-nodes (list (tabulated-list-get-id))))))

(defun teleport-list-nodes-mode--copy-nodes-id ()
  "Copy the IDs of the tagged nodes to the kill ring.
The Node ID could be used to connect to the node: tsh ssh <user>@<node-id>"
  (interactive)
  (let ((nodes-id
         (mapconcat #'identity (teleport-list-nodes--get-ids) " ")))
    (kill-new nodes-id)
    (message "Copied node ID %s to the kill ring" nodes-id)))

(defun teleport-list-nodes-mode--mark-for-command ()
  "Mark the current node for a command."
  (interactive)
  (tabulated-list-put-tag "*" t))

(defun teleport-list-nodes-mode--unmark-for-command ()
  "Remove the mark from the current node."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun teleport-list-nodes-mode--do-shell-command (command &optional nodes)
  "Execute a shell COMMAND on all NODES marked with '*' tag."
  (interactive
   (let ((nodes (teleport-list-nodes--get-ids)))
     (list
      ;; Ask the user for a command to run
      (read-shell-command (format "! on %s: " nodes))
      nodes)))
  (dolist (node-id nodes)
    (let* ((shell-command-buffer-name (format teleport-shell-command-buffer-name node-id))
           (shell-command-buffer-name-async shell-command-buffer-name))
      (shell-command (format "tsh ssh %s@%s '%s' &" (tramp-find-user teleport-tramp-method nil node-id) node-id command)
                     shell-command-buffer-name
                     shell-command-buffer-name))))

(defvar teleport-list-nodes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "k" 'teleport-list-nodes-mode--kill-column)
    (define-key map "g" 'teleport-list--refresh-buffer)
    (define-key map "i" 'teleport-list-nodes-mode--copy-nodes-id)
    (define-key map "c" 'teleport-list-nodes-mode--customize-displayed-fields)
    (define-key map "G" 'teleport-list-nodes-mode--update-list)
    (define-key map "r" 'teleport-list-nodes-mode--reset-columns)
    (define-key map "m" 'teleport-list-nodes-mode--mark-for-command)
    (define-key map "u" 'teleport-list-nodes-mode--unmark-for-command)
    (define-key map (kbd "M-!") 'teleport-list-nodes-mode--do-shell-command)
    (define-key map (kbd "/ p") 'teleport-mode--filter-by-pattern)
    (define-key map (kbd "/ r") 'teleport-mode--reset-filter-by-pattern)
    map))

(defun teleport--get-hash-map-nested (map &rest keys)
  "Get nested value from MAP by KEYS."
  (cl-loop
   for result = map then (gethash key result)
   for key in keys
   while result
   finally return result))

(defun teleport-list--get-node-label (spec prop-path)
  "Get property by PROP-PATH from node's SPEC.
Return empty string if no such property exist. If the property
contains newlines, replace them with backquoted \\n"
  (string-replace "\n" "\\n" (or (apply #'teleport--get-hash-map-nested spec prop-path) "")))

(defun teleport-list--nodes-mode-entries (nodes list-format)
  "Generate a value suitable for `tabulated-list-entries'.
Extract the values of the properties specified in LIST-FORMAT from NODES."
  (cl-loop
   for node across nodes
   for spec = (gethash "spec" node)
   for name = (gethash "name" (gethash "metadata" node))
   collect (list
            name
            (apply
             #'vector
            (cl-loop
             for p in list-format
             unless (plist-get p :hidden)
             collect (teleport-list--get-node-label spec (plist-get p :path)))))))

(defun teleport-list--format-modeline (process)
  "Return propertized FORMAT-LINE."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda () (interactive) (pop-to-buffer (process-buffer process))))

    (let ((pstatus (process-status process)))
      (propertize
       (cond
        ((memq pstatus '(run open listen connect stop)) ":Nodes(Running)")
        ((memq pstatus '(signal)) ":Nodes(Signal)")
        ((and (memq pstatus '(exit)) (= 0 (process-exit-status process))) ":Nodes")
        ((and (memq pstatus '(exit failed)) (/= 0 (process-exit-status process))) ":Nodes(Error)")
        (t ":Nodes(Unknown State)" ))
       'mouse-face 'mode-line-highlight
       'help-echo (format "mouse-1: Open process error buffer")
       'local-map map))))

(defun teleport-list--update-modeline ()
  "Update the modeline to show the status of the async processes."
  (setq mode-line-process
        (teleport-list--format-modeline teleport--nodes-async-process))
  (force-mode-line-update t))

(defun teleport-list-nodes--set-default-directory ()
  "Set the default directory to point on the remote node."
  (let (switch-default-directory)
    (map-keymap-internal (lambda (_event function)
                           (when (eq this-command function)
                             (setq switch-default-directory t)))
                         teleport-list-nodes-mode-map)
    (when switch-default-directory
      (let ((node-id (tabulated-list-get-id)))
        (setq-local default-directory
                    (format "/%s:%s@%s:"
                            teleport-tramp-method
                            (tramp-find-user teleport-tramp-method nil node-id)
                            node-id))))))


(defun teleport-list-nodes--restore-default-directory ()
  "Restore the default directory to the one used to open nodes list."
  (setq-local default-directory
              teleport-list-nodes--default-directory))

(define-derived-mode
  teleport-list-nodes-mode
  tabulated-list-mode
  "Teleport Nodes"
  "Major mode for listing the available teleport nodes.
\\<teleport-list-nodes-mode-map>
\\{teleport-list-nodes-mode-map}"
  :group 'teleport

  (unless teleport-list-nodes--default-directory
    (setq-local teleport-list-nodes--default-directory default-directory))
  (setq-local tabulated-list-padding 3)
  (add-hook 'pre-command-hook #'teleport-list-nodes--set-default-directory 90 t)
  (add-hook 'post-command-hook #'teleport-list-nodes--restore-default-directory -90 t)
  (teleport-list--refresh-buffer)
  (teleport--get-nodes-async-cached #'teleport-list--refresh-buffer))

;;;###autoload
(defun teleport-list-nodes ()
  "List all teleport nodes.
Create a buffer with a list of available teleport nodes or switch
to an existing one."
  (interactive)
  (switch-to-buffer teleport-list-nodes-buffer-name)
  (if (not (eq major-mode 'teleport-list-nodes-mode))
      (teleport-list-nodes-mode)))

(provide 'teleport)
;;; teleport.el ends here
