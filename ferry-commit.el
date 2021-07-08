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
(require 'tramp)
(require 'tabulated-list)
(require 'ferry-basic)

(defgroup ferry-commit nil
  "Show a menu of files in specific directory."
  :group 'ferry
  :group 'convenience)

(defconst ferry-commit-new-remote-dir-char ?R
  "The mark character for files to be created on the local.")

(defconst ferry-commit-new-remote-dir-string
  (char-to-string ferry-commit-new-remote-dir-char)
  "The mark string for directory to be created on the local.")

(defconst ferry-commit-new-local-dir-char ?L
  "The mark character for directory to be created on the remote.")

(defconst ferry-commit-new-local-dir-string
  (char-to-string ferry-commit-new-local-dir-char)
  "The mark string for directory to be created on the remote.")

(defconst ferry-commit-push-char ?^
  "The mark character for files to be pushed.")

(defconst ferry-commit-push-string (char-to-string ferry-commit-push-char)
  "The mark string for files to be pushed.")

(defconst ferry-commit-pull-char ?v
  "The mark character for files to be pushed.")

(defconst ferry-commit-pull-string (char-to-string ferry-commit-pull-char)
  "The mark string for files to be pushed.")

(defconst ferry-commit-bypass-char ?-
  "The mark character for files to be bypassed.")

(defconst ferry-commit-bypass-string (char-to-string ferry-commit-bypass-char)
  "The mark string for files to be pushed.")

(defvar ferry-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "_" 'ferry-commit-all-bypass)
    (define-key map "^" 'ferry-commit-select-push)
    (define-key map "v" 'ferry-commit-select-pull)
    (define-key map "-" 'ferry-commit-select-bypass)
    (define-key map "x" 'ferry-commit-commit)
    (define-key map "r" 'ferry-commit-reload)
    map)
  "Local keymap for `ferry-commit-mode' buffers.")

(defun ferry-commit-select-push ()
  "Set the control flag of current entry to `push'.
This option is applicable only to `F' type"
  (interactive)
  (-let (([ctlr type _ tlocal tremote] (tabulated-list-get-entry)))
    (when (and (> (length tlocal) 0) (string-equal type "F"))
      (tabulated-list-set-col 0 ferry-commit-push-string t)))
  (forward-line 1))

(defun ferry-commit-select-pull ()
  "Set the control flag of current entry to `pull'.
This option is applicable only to `F' type"
  (interactive)
  (-let (([ctlr type _ tlocal tremote] (tabulated-list-get-entry)))
    (when (and (> (length tremote) 0) (string-equal type "F"))
      (tabulated-list-set-col 0 ferry-commit-pull-string t)))
  (forward-line 1))

(defun ferry-commit-select-bypass ()
  "Set the control flag of current entry to `bypass'.
This option is applicable to any type"
  (interactive)
  (tabulated-list-set-col 0 ferry-commit-bypass-string t)
  (forward-line 1))

(defun ferry-commit-all-bypass ()
  "Set the control flag of all entries to `bypass'.
This option is applicable to any type"
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (tabulated-list-set-col 0 ferry-commit-bypass-string t)
    (forward-line 1)))


(define-derived-mode ferry-commit-mode tabulated-list-mode "Ferry Commit"
  "Major mode for Ferry Commit buffers."
  :interactive nil
  (ferry-mode)                          ; load the ferry mode now
  (run-mode-hooks 'ferry-commit-mode-hook))

(defun ferry-sync-buffer (current-directory)
  "Entry point of `ferry-commit-mode' with buffer set to CURRENT-DIRECTORY."
  (interactive "DDirectory to sync: ")
  (switch-to-buffer (ferry-commit--create-buffer current-directory))
  (ferry-init current-directory)
  (ferry-commit--list-files)
  (tabulated-list-print)
  (ferry-commit-mode))

(defun ferry-commit--create-buffer (current-directory)
  "Create a buffer to display a list of options for files in CURRENT-DIRECTORY."
  (let ((new-buffer (get-buffer-create "*Ferry Commit*")))
    (with-current-buffer new-buffer
      (setq-local default-directory current-directory))
    new-buffer))

(defun ferry-commit--list-files (&optional update-only)
  "Get the list of files and format it as a tabulated list.
If UPDATE-ONLY is non-nil, the tabular header will bot be set."
  (let* ((files-attrs-local
          (ferry-commit--ignore-filename
           (directory-files-and-attributes
            (ferry-local-f ferry-relative-name))))
         (files-attrs-remote
          (ferry-commit--ignore-filename
           (directory-files-and-attributes
            (ferry-remote-f ferry-relative-name))))
         (files-attrs-comb
          (alist--union files-attrs-local files-attrs-remote)))
    (ferry-commit--tabularize-list files-attrs-comb update-only)))

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
           `(,k . ,vv) `(,k . (pair ,vv . ()))))
     kvvs)))

(defun ferry-format-time-from-attr (attr)
  "Format ATTR file attribute as a time string."
  (-if-let (time (file-attribute-modification-time attr))
      (format-time-string "%F %H:%M" time) ""))

(defun ferry-format-control-from-attr (attr1 attr2)
  "Format ATTR1 and ATTR2 file attribute as a directory type."
  (let ((t1
         (-if-let (t1 (file-attribute-modification-time attr1))
             (list (car t1) (cadr t1) 0 0)))
        (t2
         (-if-let (t2 (file-attribute-modification-time attr2))
             (list (car t2) (cadr t2) 0 0)))
        (ty (ferry-format-type-from-attr attr1 attr2)))
    (cond
     ((string-equal ty "F")
      (cond ((eq nil t1) (time-less-p t1 t2)
             ferry-commit-pull-string)
            ((eq nil t2) (time-less-p t2 t1)
             ferry-commit-push-string)
            ((cond ((time-less-p t1 t2)
                    ferry-commit-pull-string)
                   ((time-less-p t2 t1)
                    ferry-commit-push-string)
                   (ferry-commit-bypass-string)))))
     ((string-equal ty "D")
      (cond ((eq nil t1) ferry-commit-new-local-dir-string)
            ((eq nil t2) ferry-commit-new-remote-dir-string)
            (t ferry-commit-bypass-string)))
     (ferry-commit-bypass-string))))

(defun ferry-format-type-from-attr (attr1 attr2)
  "Format ATTR1 and ATTR2 control based on their time."
  (defun --aux (t1)
    (cond ((eq t1 t) "D")
          ((stringp t1) "S")
          (t "F")))
  (let ((t1 (file-attribute-type attr1))
        (t2 (file-attribute-type attr2)))
    (cond ((and (eq nil attr1) attr2) (--aux t2))
          ((and (eq nil attr2) attr1) (--aux t1))
          ((eq t1 t2) (--aux t1))
          (t "?"))))

(defconst ferry-commit--time-width
  (length (format-time-string "%F %H:%M")))

(defun ferry-commit--tabularize-list (al &optional update-only)
  "Take an alist of `(pair attr1 attr2)' AL and turn it into tabulated list.

If UPDATE-ONLY is non-nil, the tabular header will bot be set."
  (let ((entries
         (-map
          (-lambda ((file . (_ attr1 . attr2)))
            (list file
                  (vector (ferry-format-control-from-attr attr1 attr2)
                          (ferry-format-type-from-attr attr1 attr2)
                          file
                          (ferry-format-time-from-attr attr1)
                          (ferry-format-time-from-attr attr2))))
          al)))
    (setq-local tabulated-list-entries entries))

  (unless update-only
    (setq-local tabulated-list-format
                (vector `("C" 1 t :pad-right 1)
                        `("T" 1 t :pad-right 1)
                        `("File" 20 t)
                        `("Local Time" ,ferry-commit--time-width t :pad-right 1)
                        `("Remote Time" ,ferry-commit--time-width t)))
    (setq-local tabulated-list-use-header-line t)
    (tabulated-list-init-header)))

(defun ferry-commit-commit ()
  "Commit the current `ferry-commit-mode'."
  (interactive)
  (let* ((entries tabulated-list-entries)
         (push-file-list
          (-filter
           (-lambda ((file [fc ft rst ...]))
             (string-equal fc ferry-commit-push-string))
           entries))
         (pull-file-list
          (-filter
           (-lambda ((file [fc ft rst ...]))
             (string-equal fc ferry-commit-pull-string))
           entries))
         (local-ndir-list
          (-filter
           (-lambda ((file [fc ft rst ...]))
             (string-equal fc ferry-commit-new-local-dir-string))
           entries))
         (remote-ndir-list
          (-filter
           (-lambda ((file [fc ft rst ...]))
             (string-equal fc ferry-commit-new-remote-dir-string))
           entries))
         (push-list-file (-map #'car push-file-list))
         (pull-list-file (-map #'car pull-file-list))
         (local-list-dir (-map #'car local-ndir-list))
         (remote-list-dir (-map #'car remote-ndir-list)))
    (let ((push-file-p (if push-list-file
                           (y-or-n-p
                            (format "Push: %s "
                                    push-list-file))))
          (pull-file-p (if pull-list-file
                           (y-or-n-p
                            (format "Pull: %s "
                                    pull-list-file))))
          (local-dir-p (if local-ndir-list
                           (y-or-n-p
                            (format "New Local Dir: %s "
                                    local-list-dir))))
          (remote-dir-p (if remote-ndir-list
                            (y-or-n-p
                             (format "New Remote Dir: %s "
                                     remote-list-dir)))))
      (when push-file-p
        (ferry-commit-batch-push push-list-file))
      (when pull-file-p
        (ferry-commit-batch-pull pull-list-file))
      (when local-dir-p
        (ferry-commit-batch-new-local-dir local-list-dir))
      (when remote-dir-p
        (ferry-commit-batch-new-remote-dir remote-list-dir)))
    (ferry-commit-reload)
    (message "Ferry Commit reloaded.")))

(defun ferry-commit--file-directory-related (f)
  (tramp-drop-volume-letter (f-join ferry-relative-name f)))

(defun ferry-commit-reload ()
  "Reload the current `ferry-commit-mode' buffer."
  (interactive)
  (ferry-commit--list-files t)
  (tabulated-list-print))

(defun ferry-commit-batch-push (push-list)
  "Push a PUSH-LIST of files from local."
  (-map #'ferry--do-push-file/sync
        (-map #'ferry-commit--file-directory-related push-list)))

(defun ferry-commit-batch-pull (pull-list)
  "Push a PULL-LIST of files from remote."
  (-map #'ferry--do-pull-file
        (-map #'ferry-commit--file-directory-related pull-list)))

(defun ferry-commit-batch-new-local-dir (local-list)
  "Make LOCAL-LIST of new directory at local."
  (-map (lambda (d) (make-directory (ferry-local-f d)))
        (-map #'ferry-commit--file-directory-related local-list)))

(defun ferry-commit-batch-new-remote-dir (remote-list)
  "Make REMOTE-LIST of new directory at remote."
  (-map (lambda (d) (make-directory (ferry-remote-f d)))
        (-map #'ferry-commit--file-directory-related remote-list)))

(defun ferry-commit--ignore-filename (l)
  "Remove ignored files from L."
  (-remove
   (-lambda ((f . rst))
     (-any (lambda (x) (string-equal f x))
           `("." ".." ,ferry-project-file)))
   l))
(provide 'ferry-commit)
;;; ferry-commit.el  ends here
