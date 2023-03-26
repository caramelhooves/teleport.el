#!/usr/bin/env -S emacs --debug --batch --load

(package-initialize)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(package-refresh-contents)
(setq package-selected-packages '(elisp-autofmt))
(package-install-selected-packages t)
(require 'elisp-autofmt)
(auto-revert-mode t)
(setq files-to-format
      (mapcar #'expand-file-name (nthcdr 3 command-line-args)))
(dolist (arg files-to-format)
  (find-file-existing (expand-file-name arg default-directory))
  (elisp-autofmt-buffer-to-file)
  (revert-buffer t t))
