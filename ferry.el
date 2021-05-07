;;; ferry.el --- Exchange files on high latency network -*- lexical-binding: t; -*-

;; Author: Mingwei Zhu <mzhu1@terpmail.umd.edu>
;; Version: 0.1
;; Package-Requires: ((f "0.20.0"))
;; Keywords: files, network
;; URL: https://github.com/mzhu65536-um/ferry-mode

;;; Commentary:
;; A simple implementation of file exchange between local and remote in high
;; lantency network settings.

;;; Code:
(require 'ferry-basic)
(require 'ferry-commit)

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

(defvar ferry-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'ferry-new-project-and-reload)
    (define-key map (kbd "r") #'ferry-reload)
    (define-key map (kbd "s") #'ferry-push-file)
    (define-key map (kbd "f") #'ferry-pull-file)
    (define-key map (kbd "n") #'ferry-sync-buffer)
    map))

(defvar ferry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ferry-mode-map-prefix ferry-command-map)
    map))

;;; Mode
;;; ###autoload
(define-minor-mode ferry-mode
  "Ferry is a minor mode to sync between local and remote ends."
  :init-value nil
  :lighter (:eval (ferry-mode-line-status))
  :keymap ferry-mode-map
  (cond
   (ferry-mode (ferry-init))))

;;; Mode -- ends here


(provide 'ferry)
;;; ferry.el  ends here
