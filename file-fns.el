;;; file-fns.el --- functions for querying about or acting on files

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1999
;; Public domain.

;; $Id: file-fns.el,v 1.9 2017/09/20 06:22:45 friedman Exp $

;;; Commentary:
;;; Code:

;;;###autoload
(defvar nf-backup-buffer-mtime-interval 3600
 "*Number of seconds from last backup before a new numbered backup file is\
 automatically created for the current buffer.

This variable is used by `nf-backup-buffer-if-mtime-elapsed', which see.")

;; This function uses `file-newest-backup', which first appears in Emacs 20.
(defun nf-backup-buffer-if-mtime-elapsed ()
"Check whether a new backup of this file should be made.

Reset the backed-up status flag for this buffer if backups are enabled and
`nf-backup-buffer-mtime-interval' seconds have elapsed since the last
backup file was made.  \(The variable `version-control' ultimately
determines whether backups are created.\)

This function should be added to `before-save-hook' to be effective."
  (condition-case err
      (let* ((bname   (file-newest-backup (buffer-file-name)))
             (mtime   (and bname (nth 5 (file-attributes bname))))
             (elapsed (and mtime (time-subtract (current-time) mtime))))
        (when (and elapsed
                   (>= (time-to-seconds elapsed)
                       nf-backup-buffer-mtime-interval))
          (setq buffer-backed-up nil)))
    (error (message "(ignored error: %s)" err))))

;;(add-hook 'before-save-hook 'nf-backup-buffer-if-mtime-elapsed)


(defvar nf-ro-backup-directory-alist
  `(("." . ,(format "~/.emacs.d/file-backups/%s" (system-name))))
  "Where to store backups for files whose directory is not writable by the current user.
This enables one to keep backups of system files instead of having all
failed backup attempts go to the single file \"~/%backup%~\".")

(defadvice make-backup-file-name-1 (around file-fns:ro-backup-directory activate)
  "If the directory for FILE cannot be written to by the current user, put
the backup in the directory specified by `nf-ro-backup-directory-alist'
\(which see\)."
  (if (file-writable-p (file-name-directory (ad-get-arg 0)))
      ad-do-it
    (let ((backup-directory-alist nf-ro-backup-directory-alist))
      ad-do-it)))

(defadvice make-auto-save-file-name (after file-fns:ro-autosave-directory activate)
  "If directory for regular autosave is not writable, autosave it in ~/.emacs.d.
More specifically, create a fully-qualified autosave file name
in ~/.emacs.d/file-backups/(system-name)/ ."
  (let* ((asf (file-name-nondirectory ad-return-value))
         (dir (file-name-directory ad-return-value)))
    (unless (file-writable-p dir)
      (let ((i 0))
        (while (< i (length dir))
          (if (char-equal (aref dir i) ?/)
              (aset dir i ?!))
          (setq i (1+ i))))
      (setq ad-return-value
            (expand-file-name
             (format "~/.emacs.d/file-backups/%s/#%s%s"
                     (system-name) dir (substring asf 1)))))))


;;;###autoload
(defun file-in-pathlist-p (file path)
  "Return path of FILE if FILE is found anywhere in PATH.
FILE is a string.  PATH is a list of strings."
  (let ((found nil)
        (f nil))
    (while (and (not found) path)
      (setq f (concat (file-name-as-directory (car path)) file))
      (and (file-exists-p f)
           (setq found f))
      (setq path (cdr path)))
    found))

;;;###autoload
(defun file-plain-p (file)
  "Returns `t' if FILE is a plain file.
That means it is not a directory, symlink, character-special device, named
pipe, socket, etc."
  (and (stringp file)
       (let ((mode (nth 8 (file-attributes file))))
         (and mode
              (= ?- (aref mode 0))))))

;;;###autoload
(defun file-name-completions-in-path (name-regexp path-list
                                                  &optional predicate filter)
  "Return an obarray containing file name completions.
All file names matching NAME-REGEXP, located in directories listed in
PATH-LIST, which satisfy optional arg PREDICATE, are put into the obarray
after being filtered through optional FILTER for potential edits.

If NAME-REGEXP is nil, then all files are candidates.

PREDICATE and FILTER should be functions which take one argument, a string
representing a file name."
  (let ((completions (make-vector 3 0))
        (files nil))
    (while path-list
      (setq files (directory-files (car path-list) nil name-regexp t))
      (while files
        (cond ((or (null predicate)
                   (funcall predicate (car files)))
               (intern (if filter
                           (funcall filter (car files))
                         (car files))
                       completions)))
        (setq files (cdr files)))
      (setq path-list (cdr path-list)))
    completions))

;;;###autoload
(defun insert-tail-of-file-contents (file bytes)
  "Insert the last N bytes of FILE.
If the file is smaller than N, just insert the entire file."
  (interactive "fFile name: \nnLast N bytes of file to insert: ")
  (setq file (expand-file-name file))
  (let* ((attr (file-attributes file))
         (size (nth 7 attr)))
    (and (= size -1)
         (signal 'overflow-error (list file size attr)))
    (if (> bytes size)
        (insert-file-contents file)
      (insert-file-contents file nil (- size bytes)))))

;;;###autoload
(defun live-find-file (filename)
  (interactive "fLive find file: ")
  (find-file filename)
  (fundamental-mode)
  (auto-revert-tail-mode 1)
  (setq buffer-read-only t)
  (goto-char (point-max)))

;;;###autoload
(defun make-autosave-for-buffer-before-kill-p ()
  (cond ((and (buffer-modified-p)
              (yes-or-no-p "This buffer is modified; make autosave? "))
         (make-local-variable 'kill-buffer-hook)
         (add-hook 'kill-buffer-hook
                    (lambda ()
                      ;; Do an auto save, then set the auto save file name
                      ;; to nil to prevent kill-buffer from deleting it.
                      ;; For some reason setting delete-auto-save-files nil
                      ;; seems not to work.
                      (if (< (emacs-version-major) 19)
                          (do-auto-save)
                        ;; In emacs 19 or later, just autosave the current buffer.
                        (do-auto-save nil t))
                      (setq buffer-auto-save-file-name nil))))
        ;; The kill-buffer-query-functions have the semantics that if any
        ;; return nil, then do not kill the buffer.  If they all return t,
        ;; then do kill.
        (t t)))

;;;###autoload
(defun make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (null current-mode)
             (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))

(provide 'file-fns)

;;; file-fns.el ends here.
