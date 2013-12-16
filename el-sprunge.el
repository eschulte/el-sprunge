;;; el-sprunge.el --- sprunge-style paste server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: elnode html sprunge paste
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'assoc)
(require 'elnode)

(defvar el-sprunge-servername "localhost"
  "Name of the server.")

(defvar el-sprunge-docroot
  (expand-file-name "scraps" elnode-config-directory)
  "Document root from which to serve Org-mode files.")

(defvar el-sprunge-before-save-hook nil
  "Hook run in a file buffer before saving web edits.
If any function in this hook returns nil then the edit is aborted.")

(defvar el-sprunge-after-save-hook nil
  "Hook run in a file buffer after saving web edits.")

(defvar el-sprunge-usage
  (format "NAME
    el-sprunge: sprunge-style paste server

SYNOPSIS
    <command> | curl -F 'sprunge=<-' %s

DESCRIPTION
    Idea and this page blatently copied from http://sprunge.us. 
    Server re-implemented in Emacs.

EXAMPLES
    ~$ cat bin/ching | curl -F 'sprunge=<-' http://%s/pb
       http://%s/42
    ~$ firefox http://%s/42
"
          el-sprunge-servername
          el-sprunge-servername
          el-sprunge-servername
          el-sprunge-servername)
  "Usage string to send for empty requests.")

(defun el-sprunge-handler (httpcon)
  (elnode-log-access "el-sprunge" httpcon)
  (elnode-method httpcon
    (GET  (el-sprunge-file-handler httpcon))
    (POST (el-sprunge-post-handler httpcon))))

(defun el-sprunge-send-usage ()
  (elnode-http-start httpcon "200" '("Content-type" . "text/plain"))
  (elnode-http-return httpcon el-sprunge-usage))

(defun el-sprunge-file-handler (httpcon)
  (let ((elnode-docroot-for-no-404 t) (elnode-docroot-for-no-cache t))
    (elnode-docroot-for el-sprunge-docroot :with file :on httpcon :do
      (el-sprunge-serve-file file httpcon))))

(defun el-sprunge-serve-file (file httpcon)
  (let* ((params (elnode-http-params httpcon))
         (path (concat file ".txt")))
    (cond
     ((string= file el-sprunge-docroot)
      (elnode-http-start httpcon "200" '("Content-type" . "text/plain"))
      (elnode-http-return httpcon el-sprunge-usage))
     ((file-exists-p path)
      (elnode-send-file httpcon path
                          :mime-types '(("text/plain; charset=utf-8" . "txt"))))
     (:otherwise (elnode-send-404 httpcon)))))

(defun el-sprunge-post-handler (httpcon)
  (let ((txt (cdr (assoc "sprunge" (elnode-http-params httpcon)))))
    (if txt
        (let ((hash (substring (sha1 txt) 0 6)))
          (with-temp-file (expand-file-name (concat hash ".txt")
                                            el-sprunge-docroot)
            (insert txt))
          (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
          (elnode-http-return httpcon hash))
      (elnode-http-start httpcon "200" '("Content-type" . "text/plain"))
      (elnode-http-return httpcon el-sprunge-usage))))

(provide 'el-sprunge)
;;; el-sprunge.el ends here
