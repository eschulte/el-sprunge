;;; el-sprunge.el --- sprunge-style paste server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: elnode html sprunge paste
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'assoc)
(require 'elnode)
(require 'htmlize)

(defcustom el-sprunge-servername "localhost"
  "Name of the server."
  :group 'el-sprunge
  :type 'string)

(defvar el-sprunge-docroot
  (expand-file-name "scraps" elnode-config-directory)
  "Document root from which to serve Org-mode files.")

(defvar el-sprunge-before-save-hook nil
  "Hook run in a file buffer before saving web edits.
If any function in this hook returns nil then the edit is aborted.")

(defvar el-sprunge-after-save-hook nil
  "Hook run in a file buffer after saving web edits.")

(defun el-sprunge-handler (httpcon)
  (elnode-log-access "el-sprunge" httpcon)
  (elnode-method httpcon
    (GET  (el-sprunge-file-handler httpcon))
    (POST (el-sprunge-post-handler httpcon))))

(defun el-sprunge-send-usage ()
  (elnode-http-start httpcon "200" '("Content-type" . "text/plain"))
  (elnode-http-return httpcon
                      (format "NAME
    el-sprunge: sprunge-style paste server

SYNOPSIS
    <command> | curl -F 'sprunge=<-' %s

DESCRIPTION
    Idea and this page blatently copied from http://sprunge.us. 
    Server re-implemented in Emacs.

EXAMPLES
    ~$ cat bin/ching | curl -F 'sprunge=<-' %s
       http://%s/a9e4e6
    ~$ firefox http://%s/a9e4e6
"
                              el-sprunge-servername
                              el-sprunge-servername
                              el-sprunge-servername
                              el-sprunge-servername)))

(defun el-sprunge-file-handler (httpcon)
  (let ((elnode-docroot-for-no-404 t) (elnode-docroot-for-no-cache t))
    (elnode-docroot-for el-sprunge-docroot :with file :on httpcon :do
      (el-sprunge-serve-file file httpcon))))

(defun el-sprunge-fontify (path as)
  (let ((new-path (concat (file-name-sans-extension path) "." as)))
    (if (not (file-exists-p path))
        new-path
      (unless (file-exists-p new-path)
        (with-temp-file new-path
          (insert-file-contents-literally path)
          (funcall (intern (concat as "-mode")))
          (font-lock-fontify-buffer)
          (insert (prog1 (with-current-buffer (htmlize-buffer) (buffer-string))
                    (delete-region (point-min) (point-max))))))
      new-path)))

(defun el-sprunge-serve-file (file httpcon)
  (let* ((as (caar (elnode-http-params httpcon)))
         (path (concat file ".txt")))
    ;; fontification
    (when (and as (string-match "^[[:alnum:]-_]\+$" as))
      (setq path (el-sprunge-fontify path as)))
    (cond
     ((string= file el-sprunge-docroot)
      (el-sprunge-send-usage))
     ((file-exists-p path)
      (elnode-send-file httpcon path
       :mime-types
       (append '(("text/plain; charset=utf-8" . "txt"))
               (when as (list (cons "text/html; charset=utf-8" as))))))
     (:otherwise (elnode-send-404 httpcon)))))

(defun el-sprunge-post-handler (httpcon)
  (let ((txt (cdr (assoc "sprunge" (elnode-http-params httpcon)))))
    (if txt
        (let ((hash (substring (sha1 txt) 0 6)))
          (with-temp-file (expand-file-name (concat hash ".txt")
                                            el-sprunge-docroot)
            (insert txt))
          (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
          (elnode-http-return
           httpcon
           (format "http://%s/%s\n" el-sprunge-servername hash)))
      (el-sprunge-send-usage))))

(provide 'el-sprunge)
;;; el-sprunge.el ends here
