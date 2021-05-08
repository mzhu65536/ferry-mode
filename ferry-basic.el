;;; ferry-basic.el --- Exchange files on high latency network -*- lexical-binding: t; -*-

;; Author: Mingwei Zhu <mzhu1@terpmail.umd.edu>
;; Version: 0.1
;; Package-Requires: ((f "0.20.0"))
;; Keywords: files, network
;; URL: https://github.com/mzhu65536-um/ferry-mode

;;; Commentary:
;; A simple implementation of file exchange between local and remote in high
;; lantency network settings.

;;; Code:
(require 'tramp)
(require 'dash)
(require 's)                            ; sexp
(require 'f)                            ; file ops

;;; Customization
(defgroup ferry nil
  "Exchange files on high latency network"
  :prefix "ferry-"
  :group 'tools)

(defcustom ferry-project-file ".ferry-project"
  "Project file name indicating the roots of the local and remote.

The location of the file is the root of the local copy.  The first line in the
project file indicates the directory of the remote."
  :group 'ferry
  :type '(string)
  :safe #'ferry-project-file-p)

(defcustom ferry-mode-hook nil
  "Hooks to run after command `ferry-mode' is loaded."
  :group 'ferry
  :type 'hook
  :risky t)

(defcustom ferry-mode-line-prefix "FR-"
  "Lighter prefix of `ferry-mode'."
  :group 'ferry
  :type '(string)
  :risky t)

(defcustom ferry-mode-map-prefix (kbd "C-c f")
  "Map prefix of `ferry-mode'."
  :group 'ferry
  :risky t)

;;; Customization -- ends here

;;; Local variables
(defvar-local ferry-status nil
  "Status of `ferry-mode' for mode line processing.
One of `local-unloaded' `local-loaded' `remote' `non-file'")

;;; ferry-status template
;; (pcase <>
;;   ('local-unloaded ...)
;;   ('local-loaded ...)
;;   ('remote ...)
;;   ('non-file ...))

(defvar-local ferry-buffer-name nil
  "The file/directory name of current buffer.")
(defvar-local ferry-local-root nil
  "Local root of ferry project.")
(defvar-local ferry-remote-root nil
  "Remote root of ferry project.")
(defvar-local ferry-relative-name  nil
  "Relative path from the local root of the ferry project.")

;;; Local variables -- ends here

(defun ferry-remote-f (f)
  "Translate relative filename [F] into root based."
  (tramp-drop-volume-letter (f-join ferry-remote-root f)))

(defun ferry-local-f (f)
  "Translate relative filename [F] into local based."
  (f-join ferry-local-root f))

(defun ferry-relate-name (f)
  "Translate a filename F into relative filename to the root."
  (f-relative f ferry-local-root))

(defun ferry-project-file-p (f)
  "Determine if [F] is a filename."
  ;; TODO : blacklist the use of illegal chars in filenames
  (stringp f))

;;; Core
(defun ferry-init (&optional name)
  "Initialize the ferry mode.

NAME can be optionally provided to override the default file/dir name."
  (if name (setq-local ferry-buffer-name name)
    (let ((local-name buffer-file-truename)
          (local-dir default-directory))
      (if local-name (setq-local ferry-buffer-name local-name)
        (if local-dir (setq-local ferry-buffer-name local-dir)))))
  (if ferry-buffer-name
      (if (file-remote-p ferry-buffer-name)
          (setq-local ferry-status 'remote)
        (let ((pdir (ferry-search-project-file)))
          (if pdir
              (progn
                (setq-local ferry-status 'local-loaded
                            ferry-local-root pdir
                            ferry-remote-root
                            (f-read (f-join pdir ferry-project-file)))
                (setq-local ferry-relative-name
                            (ferry-relate-name ferry-buffer-name)))
            (setq-local ferry-status 'local-unloaded))))
    (setq-local ferry-status 'non-file)))

;;; TODO : Monitor file change and auto reload.
(defun ferry-reload ()
  "Reload the ferry mode in current buffer.

Essential for user to reload the mode after editing the project file."
  (interactive)
  (setq-local ferry-buffer-name nil
              ferry-status nil
              ferry-local-root nil
              ferry-remoe-root nil
              ferry-relative-name nil)
  (ferry-init))

(defun ferry-new-project-and-reload (local-root remote-root)
  "Create a project file LOCAL-ROOT containing REMOTE-ROOT in the first line."
  (interactive "DLocal project root: \nsRemote project root: ")
  (write-region remote-root nil (f-join local-root ferry-project-file) nil)
  (ferry-reload))

(defun ferry-search-project-file ()
  "Search for the ferry project file from current buffer.

Returns the project file directory if found, nil otherwise."
  (letrec ((loop-aux
            (lambda (dir)
              (progn
                (let ((filename (f-join dir ferry-project-file)))
                  (if (f-exists? filename)
                      (f-full dir)
                    (unless (f-root? dir)
                      (funcall loop-aux (f-parent dir)))))))))
    (funcall loop-aux
             (if (f-dir-p ferry-buffer-name)
                 ferry-buffer-name
               (file-name-directory ferry-buffer-name)))))

(defun ferry-push-file ()
  "Upload the file to the remote end."
  (interactive)
  (if (eq ferry-status 'local-loaded)
      (ferry--do-push-file ferry-relative-name)
    (ferry-fail-message)))

(defun ferry--do-push-file (filename)
  "Upload the FILENAME to the remote end."
  (copy-file (ferry-local-f filename)
             (ferry-remote-f filename)
             t t))

(defun ferry-pull-file ()
  "Download the remote version and replace the current buffer."
  (interactive)
  (if (eq ferry-status 'local-loaded)
      (progn
        (ferry--do-push-file ferry-relative-name)
        (revert-buffer t t))
    (ferry-fail-message)))

(defun ferry--do-pull-file (filename)
  "Upload the FILENAME to the remote end."
  (copy-file (ferry-remote-f filename)
             (ferry-local-f filename)
             t t))
;;; Miscs
(defun ferry-fail-message ()
  "Print mode related errors in `ferry-mode'."
  (message
   (pcase ferry-status
     ('local-unloaded "Ferry project is not properly loaded!")
     ('local-loaded "Seriously? Please report bug!")
     ('remote "Ferry can only be used on the local buffer!")
     ('non-file "Ferry is not supported on this kind of buffer."))))



(provide 'ferry-basic)
;;; ferry-basic.el  ends here
