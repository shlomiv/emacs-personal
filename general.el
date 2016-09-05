;;; GENERAL --- Summary
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

;;; a workaround for https://github.com/capitaomorte/yasnippet/issues/289
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))

(when (require 'yasnippet nil 'noerror)
  (yas-global-mode 1)
  (progn
    (yas/load-directory "~/.emacs.d/snippets")))

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:" "/ssh:asr"))))

;;; open files as root support
(defun shlomi/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;;; in elisp - eval and replace (+ 1 2 3) => 6
(defun shlomi/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun shlomi/kill-line-or-region ()
  "kill region if active only or kill line normally"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'prelude-kill-whole-line)))

(with-eval-after-load 'ido
  (setq ido-use-faces 't)

  (custom-set-faces
   '(ido-subdir ((t (:foreground "LightGreen")))) ;; Face used by ido for highlighting subdirs in the alternatives.
   '(ido-first-match ((t (:foreground "Darkolivegreen3")))) ;; Face used by ido for highlighting first match.
   '(ido-only-match ((t (:foreground "#ffcc33")))) ;; Face used by ido for highlighting only match.
   '(ido-indicator ((t (:foreground "#ffffff")))) ;; Face used by ido for highlighting its indicators (don't actually use this)
   '(ido-incomplete-regexp ((t (:foreground "#ffffff")))))
  )

;; Stop the annoying 'errors' on elisp comments! sheesh!
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general.el ends here
