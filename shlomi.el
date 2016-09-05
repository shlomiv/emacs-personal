;;; visuals

(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)

(defun they-to-us (they)
  (mod (+ they -10) 24))

(defun us-to-they (they)
  (mod (+ they 10) 24))

(defun shlomi/israel-from-san-jose ()
  (interactive)
  (message (concat "time in israel: " (int-to-string (us-to-they (string-to-int (read-from-minibuffer "time here: ")))))))

(defun shlomi/san-jose-from-israel ()
  (interactive)
  (message (concat "time in san-jose: " (int-to-string (they-to-us (string-to-int (read-from-minibuffer "time there: ")))))))

(set-register ?c (cons 'file "~/.ssh/config"))
;; open with C-x r j c

(defun shlomi/get-ssh-hosts ()
  "Retrieve all ssh host from local configuration files."
  (-map (lambda (s) (list (replace-regexp-in-string ":$" "" s)))
        (let ((tramp-completion-mode t))
          (tramp-completion-handle-file-name-all-completions "" "/ssh:"))))


(defun shlomi/select-endpoint ()
  "Interactively select the host and port to connect to."
  (let* ((ssh-hosts (shlomi/get-ssh-hosts))
         (hosts ssh-hosts)
         (sel-host (cider--completing-read-host hosts))
         (host (car sel-host))
         (port (or (cadr sel-host)
                   (cider--completing-read-port host (cider--infer-ports host ssh-hosts)))))
    (list host)))

;;
(eval-when-compile
  (when (not (fboundp 'save-mark-and-excursion))
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))


(defun shlomi/kill-all-cider-buffers ()
  (interactive)
  (mapcar 'kill-buffer
          (remove-if-not
           (lambda (x)
             (string-match ".*cider.*" (buffer-name x)))
           (buffer-list))))

(defun shlomi/kill-all-nrepl-buffers ()
  (interactive)
  (mapcar 'kill-buffer
          (remove-if-not
           (lambda (x)
             (string-match ".*nrepl.*" (buffer-name x)))
           (buffer-list))))

(defun shlomi/update-cluster-ip (ip)
  (interactive "snew ip:")
  (when (not (string= "" ip))
    (find-file "~/.ssh/config")
    (goto-char (point-min))
    (search-forward "ganglia")
    (search-forward "hostname")
    (kill-line)
    (insert " ")
    (insert ip))
  )

(defun shlomi/fix-tab-table (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((text (buffer-substring-no-properties beg end))
         (text (replace-regexp-in-string "\\(^\\)." "|" text nil nil 1))
         (text (replace-regexp-in-string ".\\($\\)" "|" text nil nil 1))
         (text (replace-regexp-in-string "\t" "|" text))
         )
    (delete-active-region)

    (string-match "^" text (string-match "|$" text))
    (insert (replace-match "|-\n" nil nil text))
    (when (eq major-mode 'org-mode)
      (org-table-align))
    ))



;;(require 'spaceline-config)
;;(setq mode-line-format (spaceline-spacemacs-theme))


;;;(defun shlomi/update-ssh-config-host (hostname)
;;;  (interactive (shlomi/select-endpoint)))
;;;
;;;
;;;(cider-connect)
;;;(shlomi/select-endpoint)
;;;
;;;(shlomi/update-ssh-config-host)
;;;

;;(setq sml/theme 'light)
;;(setq sml/theme 'respectful)
(setq sml/theme 'dark)
;;t

;;(powerline-center-theme)

(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(defun prelude-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))
