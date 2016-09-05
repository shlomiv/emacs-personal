;;; KEYBINDINGS --- Summary
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

(prelude-require-package 'multiple-cursors)
(prelude-require-package 'phi-search)
(prelude-require-package 'ace-jump-mode)
(prelude-require-package 'helm-core)
(prelude-require-package 'rainbow-identifiers)
(prelude-require-package 'fancy-narrow)
(prelude-require-package 'inf-clojure)
(prelude-require-package 'ensime)

(require 'multiple-cursors)
(require 'ace-jump-mode)


;; enable origami mode
(global-origami-mode)

;;; Key bindings

;; Origami - folds code
(global-set-key (kbd "<C-tab>") 'origami-toggle-all-nodes)
(global-set-key (kbd "<S-tab>") 'origami-toggle-node)

;; I often like to replace elisp sexps with its result
(global-set-key (kbd "C-c C-e") 'shlomi/eval-and-replace)
(global-set-key (kbd "C-c j") 'eval-print-last-sexp)

;; multiple cursor stuff
(global-set-key (kbd "C-;")     'mc/mark-all-like-this)
(global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
(global-set-key (kbd "C->")     'mc/mark-next-like-this)
(global-set-key (kbd "C-\"")    'mc/edit-lines)

;; conveniently kill region if one is active, if not, simply kill current line
(global-set-key (kbd "C-w")   'shlomi/kill-line-or-region)

;; Unset swapping meta and super, this is being done on the OS
(define-key prelude-mode-map (kbd "C-c w") 'nil)

;; nice stuff grabbed from emacs-live
(define-key clojure-mode-map (kbd "C-:") 'shlomi/live-toggle-clj-keyword-string)
(define-key clojure-mode-map (kbd "M-[") 'shlomi/live-cycle-clj-coll)

;; One of my favorite shortcuts
;; (global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "s-w") 'ace-window)
(global-set-key (kbd "s-s") 'avy-isearch)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "s-<up>") 'avy-goto-line-above)
(global-set-key (kbd "s-<down>") 'avy-goto-line-below)
(global-set-key (kbd "s-c") 'avy-copy-line)
(global-set-key (kbd "s-M") 'avy-move-line)
(global-set-key (kbd "s-d") 'avy-copy-region) ;; not doing what I thought...
(global-set-key (kbd "s-SPC") 'avy-goto-char)
(global-set-key (kbd "s-2") 'avy-goto-char-2)
(global-set-key (kbd "s-1") 'avy-goto-char)


;; ahs movements
(global-set-key (kbd "C-.") 'ahs-forward)
(global-set-key (kbd "C-,") 'ahs-backward)
(global-set-key (kbd "C-x C-.") 'ahs-change-range)
(global-set-key (kbd "C-s-/") 'ahs-edit-mode)
(global-set-key (kbd "C-s-.") 'ahs-forward-definition)
(global-set-key (kbd "C-s-,") 'ahs-backward-definition)

;; The inverse of M-^, this joins bottom lines to current line
(global-set-key (kbd "C-M-^") '(lambda() (interactive) (delete-indentation t)))

;; phi-search
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings.el ends here
