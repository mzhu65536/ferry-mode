;;; ferry.el --- Exchange files on high latency network -*- lexical-binding: t; -*-

;; Author: Mingwei Zhu <mzhu1@terpmail.umd.edu>
;; Version: 0.1
;; Package-Requires: ((f "0.20.0"))
;; Keywords: files, network
;; URL: https://www.github.com/mzhu65536-um/ferry

;;; Commentary:
;; A simple implementation of file exchange between local and remote in high
;; lantency network settings.

;;; Code:
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

(defvar-local ferry-local-root nil
  "Local root of ferry project.")
(defvar-local ferry-remote-root nil
  "Remote root of ferry project.")
(defvar-local ferry-relative-name  nil
  "Relative path from the local root of the ferry project.")
(defvar-local ferry-hier nil
  "Hierachy of the local ferry project.")
;;; Local variables -- ends here

(defun ferry-remote-f (f)
  "Translate relative filename [F] into root based."
  (f-join ferry-remote-root f))

(defun ferry-local-f (f)
  "Translate relative filename [F] into local based."
  (f-join ferry-local-root f))

(defun ferry-project-file-p (f)
  "Determine if [F] is a filename."
  ;; TODO : blacklist the use of illegal chars in filenames
  (stringp f))

;;; Core
(defun ferry-new-project ()
  "Create a new ferry project file."
  (interactive)
  (let ((local-root
         (read-file-name
          "Local project root : "
          nil
          (f-dirname (buffer-file-name))))
        (remote-root
         (read-file-name
          "Remote project root : ")))
    (write-region remote-root nil local-root nil)))

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
    (funcall loop-aux (file-name-directory (buffer-file-name)))))

;; TODO : let's not let this function crash at anytime
;; `buffer-file-name' crashes at Dired mode
(defun ferry-init ()
  "Initialize the ferry mode."
  (interactive)
  (let ((local-name buffer-file-truename))
    (if local-name
        (if (file-remote-p local-name)
            (setq-local ferry-status 'remote)
          (let ((pdir (ferry-search-project-file)))
            (if pdir
                (progn
                  (setq-local ferry-status 'local-loaded
                              ferry-local-root pdir
                              ferry-remote-root (f-read (f-join pdir
                                                                ferry-project-file)))
                  (setq-local ferry-relative-name (f-relative local-name
                                                              ferry-local-root)))
              (setq-local ferry-status 'local-unloaded))))
      (setq-local ferry-status 'non-file))))

(defun ferry-push-file ()
  "Upload the file to the remote end."
  (interactive)
  (if (eq ferry-status 'local-loaded)
      (copy-file (buffer-file-name)
                 (ferry-remote-f ferry-relative-name)
                 t t)
    (ferry-fail-message)))

(defun ferry-pull-file ()
  "Download the remote version and replace the current buffer."
  (interactive)
  (if (eq ferry-status 'local-loaded)
      (progn
        (copy-file (ferry-remote-f ferry-relative-name)
                   (buffer-file-name)
                   t t)
        (revert-buffer t t))
    (ferry-fail-message)))

;; (defun ferry-sync-dir ()
;;   "Synchronize the local directory with the remote.

;; However, it is the users' responsibility to reload buffer from disk."
;;   (let* ((dir (read-directory-name "Directory to sync: " nil
;;                                    (f-dirname ferry-relative-name)))
;;          (file-attr-contained (directory-files-and-attributes
;;                                (ferry-remote-f dir)))
;;          (files-new-old (split-files (cdr (cdr file-attr-contained))))
;;          (files-new-commit (read-file-name "Remote -> Local: " nil
;;                                            (car files-new-old)))
;;          (files-old-commit (read-file-name "Local -> Remote: " nil
;;                                            (cdr (files-old-new)))))
;;     (-map (lambda (f) (copy-file (ferry-remote-f f) (ferry-local-f f) t t))
;;           files-to-sync)))

;;; Miscs
(defun ferry-fail-message ()
  "Print mode related errors in `ferry-mode'."
  (message
   (pcase ferry-status
     ('local-unloaded "Ferry project is not properly loaded!")
     ('local-loaded "Seriously? Please report bug!")
     ('remote "Ferry can only be used on the local buffer!")
     ('non-file "Ferry is not supported on this kind of buffer."))))

(defun ferry-mode-line-status ()
  "Mode line of ferry mode."
  (pcase-let*
      ((`(,suffix ,face ,help)
        (cl-case ferry-status
          ('local-unloaded
           `("Local" (face (:inherit warning)) "Cannot find project root."))
          ('local-loaded
           `(,ferry-local-root (face (:inherit success)) "Project root found."))
          ('remote
           `("Remote" (face (:inherit error)) "Remote file will not be processed."))
          ('non-file
           `("?" (face (:inherit error)) "Non-standard file buffer.")))))
    `(" " (:propertize ,(concat ferry-mode-line-prefix suffix)
                       ,@face
                       help-echo ,help))))

;; (defun ferry-mode-line-status2 ()
;;   "Mode line of ferry mode."
;;   `(" " ,ferry-mode-line-prefix "12345"))

(defvar ferry-mode-line
  '(:eval (ferry-mode-line-status2)))

(defconst ferry-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'ferry-new-project)
    (define-key map (kbd "r") #'ferry-init)
    (define-key map (kbd "s") #'ferry-push-file)
    (define-key map (kbd "f") #'ferry-pull-file)))

;; (defvar ferry-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map ferry-mode-map-prefix ferry-command-map)))

;;; Mode
;;; ###autoload
(define-minor-mode ferry-mode
  "Ferry is a minor mode to sync between local and remote ends."
  :init-value nil
  :lighter (:eval (ferry-mode-line-status))
  ;; :keymap ferry-mode-map
  (cond
   (ferry-mode (ferry-init))))

;;; Mode -- ends here

(provide 'ferry)
;;; ferry.el  ends here
