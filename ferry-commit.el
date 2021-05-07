;;; ferry-commit.el --- Interface for selecting files -*- lexical-binding: t; -*-

;; Author: Mingwei Zhu <mzhu1@terpmail.umd.edu>
;; Version: 0.1
;; Package-Requires: ((f "0.20.0"))
;; Keywords: files, network
;; Packege: ferry
;; URL: https://github.com/mzhu65536-um/ferry-mode

;;; Commentary:
;; Interface for selecting files.
;; To be using to prompt which files to be sync and how they will be synced.

;;; Code:
(require 'tabulated-list)
(require 'ferry-basic)

(defgroup ferry-commit nil
  "Show a menu of files in specific directory."
  :group 'ferry
  :group 'convenience)

(defconst ferry-commit-push-char ?^
  "The mark character for files to be pushed.")

(defconst ferry-commit-pull-char ?v
  "The mark character for files to be pushed.")

(defconst ferry-commit-bypass-char ?-)

(defvar ferry-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "^" 'ferry-commit-select-push)
    (define-key map "v" 'ferry-commit-select-pull)
    (define-key map "-" 'ferry-commit-select-bypass)
    (define-key map "x" 'ferry-commit-commit)
    map)
  "Local keymap for `ferry-commit-mode' buffers.")


(define-derived-mode ferry-commit-mode tabulated-list-mode "Ferry Commit"
  "Major mode for Ferry Commit buffers."
  :interactive nil
  t)

(defun ferry-sync-buffer (current-directory)
  "Entry point of `ferry-commit-mode' with buffer set to CURRENT-DIRECTORY."
  (interactive "DDirectory to sync: ")
  (switch-to-buffer (ferry-commit--create-buffer current-directory))
  (ferry-commit-mode))

(defun ferry-commit--create-buffer (current-directory)
  "Create a buffer to display a list of options for files in CURRENT-DIRECTORY."
  (let ((new-buffer (get-buffer-create "*Ferry Commit*")))
    (with-current-buffer new-buffer
      (ferry-init current-directory)
      (ferry-commit--list-files)
      (tabulated-list-print))
    new-buffer))

(defun ferry-commit--list-files ()
  "Get the list of files and format it as a tabulated list."
  (let* ((files-attrs-local
          (directory-files-and-attributes (ferry-local-f ferry-relative-name)))
         (files-attrs-remote
          (directory-files-and-attributes (ferry-remote-f ferry-relative-name)))
         (files-attrs-comb
          (alist--union files-attrs-local files-attrs-remote)))
    (ferry-commit--tabularize-list files-attrs-comb)))

(defun alist--union (al1 al2)
  "Take the union of two lists (AL1 and AL2) of files and attributes."
  (let ((kvvs
         (-reduce-from
          (-lambda (kvvs (k2 . v2))
            (-let (((k1 . v1) (assoc k2 kvvs)))
              (cons `(,k2 . (pair ,v1 . ,v2))
                    (if v1 (assoc-delete-all k2 kvvs) kvvs))))
          al1 al2)))
    (-map
     (-lambda ((k . vv))
       (if (and (consp vv) (eq 'pair (car vv)))
           `(,k . ,vv) `(,k . (pair ,vv ()))))
     kvvs)))

(defun ferry-format-time-from-attr (attr)
  "Format ATTR file attribute as a time string."
  (-if-let (time (file-attribute-modification-time attr))
      (format-time-string "%F %H:%M" time) ""))

(defconst ferry-commit--time-width
  (length (current-time-string)))

(defun ferry-commit--tabularize-list (al)
  "Take an alist of `(pair attr1 attr2)' AL and turn it into tabulated list."
  (let ((entries
         (-map
          (-lambda ((file . (_ attr1 . attr2)))
            (list file
                  (vector "?" file
                          (ferry-format-time-from-attr attr1)
                          (ferry-format-time-from-attr attr2))))
          al)))
    (setq-local tabulated-list-format
                (vector '("F" 1 t :pad-right 1)
                        `("File" 20 t)
                        `("Local Time" ,ferry-commit--time-width t :pad-right 1)
                        `("Remote Time" ,ferry-commit--time-width t)))
    (setq-local tabulated-list-use-header-line t)
    (setq-local tabulated-list-entries entries))
  (tabulated-list-init-header))

(provide 'ferry-commit)
;;; ferry-commit.el  ends here
