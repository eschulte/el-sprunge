;;; el-sprunge.el --- sprunge-style paste server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: elnode html sprunge paste
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server)
(require 'htmlize)
(require 'cl-lib)

(defcustom el-sprunge-servername "localhost"
  "Name of the server."
  :group 'el-sprunge
  :type 'string)

(defvar el-sprunge-docroot
  (expand-file-name "scraps" (file-name-directory
                              (or load-file-name (buffer-file-name))))
  "Document root from which to serve Org-mode files.")

(defvar el-sprunge-after-save-hook nil
  "Hook run in a file buffer after saving a new post.")

(defvar el-sprunge-handler
  '(((:GET  . "^/$") . el-sprunge-send-usage)
    ((:GET  . ".*")  . el-sprunge-file-handler)
    ((:POST . ".*")  . el-sprunge-post-handler)))

(defun el-sprunge-send-usage (proc request)
  (ews-response-header proc 200
    '("Content-type" . "text/plain; charset=utf-8"))
  (process-send-string proc
    (format "NAME
    el-sprunge: sprunge-style command line paste server

SYNOPSIS
    <command> | curl -s -F 'sprunge=<-' %s

DESCRIPTION
    Idea and this page blatently copied from http://sprunge.us. 
    Server re-implemented in Emacs.

EXAMPLES
    ~$ cat bin/ching | curl -s -F 'sprunge=<-' %s
       http://%s/a9e4e6
    ~$ firefox http://%s/a9e4e6
"
            el-sprunge-servername
            el-sprunge-servername
            el-sprunge-servername
            el-sprunge-servername))
  :finished)

(defun el-sprunge-subdirectory-p (parent dir)
  (let* ((expanded (expand-file-name dir))
         (complete (if (string= (substring expanded -1) "/")
                       expanded
                     (concat expanded "/"))))
    (and (>= (length complete) (length parent))
         (string= parent (substring complete 0 (length parent)))
         complete)))

(defun el-sprunge-404 (proc)
  (ews-response-header proc 404
    '("Content-type" . "text/plain"))
  (process-send-string proc "404 not found")
  :finished)

(defun el-sprunge-file-handler (proc request)
  (let ((path (concat el-sprunge-docroot (cdr (assoc :GET request)))))
    (if (el-sprunge-subdirectory-p el-sprunge-docroot path)
        (el-sprunge-serve-file path proc request)
      (el-sprunge-404 proc))))

(defun el-sprunge-fontify (path as)
  (let ((new-path (concat (file-name-sans-extension path) "." as))
        (enable-local-variables nil))
    (if (not (file-exists-p path))
        new-path
      (unless (file-exists-p new-path)
        (with-temp-file new-path
          (insert-file-contents-literally path)
          (funcall (intern (concat as "-mode")))
          (font-lock-fontify-buffer)
          (insert (let ((html-buffer (htmlize-buffer)))
                    (prog1 (with-current-buffer html-buffer (buffer-string))
                      (kill-buffer html-buffer)
                      (delete-region (point-min) (point-max)))))))
      new-path)))

(defun el-sprunge-serve-file (uri proc request)
  (let (path as)
    (when (string-match "?" uri)
      (setq path (substring uri 0 (match-beginning 0))
            as   (substring uri (match-end 0))))
    (setq path (concat path ".txt"))
    ;; fontification
    (when (and as (string-match "^[[:alnum:]-_]\+$" as))
      (setq path (el-sprunge-fontify path as)))
    (cond
     ((file-exists-p path)
      (ews-response-header proc 200
        '("Content-type" . (if as
                               "text/html; charset=utf-8"
                             "text/plain; charset=utf-8")))
      (process-send-string proc
        (with-temp-buffer
          (insert-file-contents-literally path)
          (buffer-string))))
     (:otherwise (el-sprunge-404 proc)))
    :finished))

(defun el-sprunge-post-handler (proc request)
  (let ((txt (cdr (assoc 'content (cdr (assoc "sprunge" request))))))
    (if txt
        (let* ((hash (substring (sha1 txt) 0 6))
               (path (expand-file-name (concat hash ".txt")
                                       el-sprunge-docroot)))
          (with-temp-file path (insert txt))
          (when el-sprunge-after-save-hook
            (find-file-literally path)
            (run-hooks 'el-sprunge-after-save-hook)
            (kill-buffer))
          (ews-response-header proc 200 '("Content-type" . "text/plain;"))
          (process-send-string proc
            (format "http://%s/%s\n" el-sprunge-servername hash))
          :finished)
      (el-sprunge-send-usage proc request))))

(provide 'el-sprunge)
;;; el-sprunge.el ends here
