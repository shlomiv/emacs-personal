;;; CLOJURE --- Summary
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
;;(require 'ac-nrepl)
;;(require 'auto-complete-config)
(require 'prelude-clojure)
(require 'tramp)
(prelude-require-package 'multiple-cursors)
(require 'multiple-cursors)
(prelude-require-package 'ace-jump-mode)
(require 'ace-jump-mode)
;;(require 'mic-paren)

(prelude-require-package 'clj-refactor)
(require 'clj-refactor)

(prelude-require-package 'cljr-helm)
(require 'cljr-helm)

(prelude-require-package 'auto-highlight-symbol)
(require 'auto-highlight-symbol)

;;(prelude-require-package 'origami)
;;(require 'origami)

;; Activate Highlighting of matching parentheses
;;(paren-activate)

;;; set special chars
(defun lambda-as-lambda (mode ch pattern)
  (font-lock-add-keywords
   mode `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,ch 'decompose-region)))))))
;; Setup lambdas
(lambda-as-lambda 'clojure-mode "λ" "(\\(\\<fn\\>\\)")
(lambda-as-lambda 'clojure-mode "λ" "(\\(fn\\)[\[[:space:]]")
(lambda-as-lambda 'clojure-mode "ƒ" "\\(#\\)(")
(lambda-as-lambda 'clojure-mode "∈" "\\(#\\){")
(lambda-as-lambda 'emacs-lisp-mode  "λ" "(\\(\\<lambda\\>\\)")

;; configure what test files looks like
(defun clojure-test-filename ()
  (concat (projectile-project-root)
          "test/"
          (mapconcat #'identity
                     (butlast (split-string (cider-current-ns) "\\.")) "/")
          "/"
          (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
          "_test.clj"))

(defadvice projectile-toggle-between-implementation-and-test (around create-clojure-test-advice)
  "Visit new file if can't find test."
  (condition-case nil
      ad-do-it
    (error (find-file (clojure-test-filename)))))

(ad-activate 'projectile-toggle-between-implementation-and-test)

;; configure clojure refactory
                                        ; install the missing packages

;;(defun my-clojure-mode-hook ()
;;  (clj-refactor-mode 1)
;;  (yas-minor-mode 1) ; for adding require/use/import
;;  (cljr-add-keybindings-with-prefix "C-x C-a"))

;;(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1) ; for adding require/use/import
                               ;; insert keybinding setup here
                               ;;(cljr-add-keybindings-with-prefix "C-x C-a")
                               (local-set-key  (kbd "C-x C-a") 'cljr-helm)
                               ;;(origami-mode 1)
                               ))



;;(require 'highlight)
;;(require 'nrepl-eval-sexp-fu)
;;(setq nrepl-eval-sexp-fu-flash-duration 0.5)
;;
;;(set-face-attribute 'nrepl-eval-sexp-fu-flash
;;                    nil
;;                    :foreground nil
;;                    :background nil
;;                    :inverse-video t
;;                    :weight 'bold)

;;; cool stuff taken from emacs-live
(defun shlomi/live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun shlomi/live-toggle-clj-keyword-string ()
  "convert the string or keyword at (point) from string->keyword or keyword->string."
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake."))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (shlomi/live-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (shlomi/live-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun shlomi/live-cycle-clj-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> (x) recur"
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (shlomi/live-delete-and-extract-sexp) 1 -1) "}"))
     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "[" (substring (shlomi/live-delete-and-extract-sexp) 1 -1) "]"))
     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "(" (substring (shlomi/live-delete-and-extract-sexp) 1 -1) ")"))
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

;; Configure cider eldoc
;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
;; auto-highlight config

;; remove default bindings from mode-map
(eval-after-load 'auto-highlight-symbol
  (progn
    (define-key auto-highlight-symbol-mode-map (kbd "M-<left>" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M-<right>" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "M--" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "C-x C-'" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "C-x C-a" ) nil)
    (define-key auto-highlight-symbol-mode-map (kbd "<S-left>") nil)))

;; set up defaults
(setq ahs-case-fold-search nil
      ahs-default-range 'ahs-range-whole-buffer ;; start with scanning the entire buffer
      ahs-idle-timer 10000000                   ;; Make the timer very long
      ahs-idle-interval 100000000               ;; Make the timer very long
      ahs-inhibit-face-list nil)

(defun prepare-ahs()
  "Ensures that auto-highlight-mode is enabled and highlithing is active"
  (auto-highlight-symbol-mode t)
  (ahs-highlight-now))

;; advice to start the mode and activate highlithing
(advice-add 'ahs-forward :before #'prepare-ahs)
(advice-add 'ahs-backward :before #'prepare-ahs)
(advice-add 'ahs-forward-definition :before #'prepare-ahs)
(advice-add 'ahs-backward-definition :before #'prepare-ahs)

;; add cider/clojure modes to ahs's modes
(add-to-list 'ahs-modes 'cider-mode)
(add-to-list 'ahs-plugin-bod-modes 'cider-mode)
(add-to-list 'ahs-plugin-bod-modes 'clojure-mode)

;; activate it globally
(global-auto-highlight-symbol-mode t)

(setq cider-prompt-save-file-on-load 'always-save)

(setq clojure-docstring-fill-column 120)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure.el ends here
