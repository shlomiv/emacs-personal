;;; NETWORK --- Summary
;;
;; Author: Shlomi Vaknin <vaknins9@vaknins9-mac01.local>
;; Copyright © 2015, Intel, Shlomi Vaknin, all rights reserved.
;; Created: 14 April 2015
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'tramp)

;; Interactively enable/disable intel proxy
(defun shlomi/intel-proxy-enable ()
  (interactive)
  (setq url-proxy-services '(("http" . "proxy.iil.intel.com:911")
                             ("https" . "proxy.iil.intel.com:911"))))

(defun shlomi/intel-proxy-disable ()
  (interactive)
  (setq url-proxy-services '()))

;;; tramp defaults
(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
(setq tramp-default-method "scp")

;;(add-to-list 'load-path "~/.emacs.d/tramp/lisp/")
;;(add-to-list 'tramp-default-proxies-alist  '("ec2-107-22-50-177.compute-1.amazonaws.com" nil "/ssh:ubuntu@ec2-107-22-50-177.compute-1.amazonaws.com:"))

;; Configure tramp to use ssh and parse .ssh/config file for convenience
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

;; keep passwords
(setq password-cache-expiry nil)

;; remote path to start on
(add-to-list 'tramp-remote-path "/home/ubuntu/")

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;;; Status bar mods - if a file is remote, show where its at on the status bar
(defconst my-mode-line-buffer-identification
  (list
   '(:eval
     (let ((host-name
            (if (file-remote-p default-directory)
                (concat
                 (propertize
                  (tramp-file-name-host (tramp-dissect-file-name default-directory))
                  'font-lock-face '(:foreground "light green" :weight bold)) ":")
              "")))
       (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
           (substring host-name 0 (match-beginning 1))
         host-name)))
   "%12b"))

(setq-default mode-line-buffer-identification my-mode-line-buffer-identification)

(add-hook
 'prog-mode-hook
 '(lambda ()
    (setq
     mode-line-buffer-identification
     my-mode-line-buffer-identification)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; network.el ends here
