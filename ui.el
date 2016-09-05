;;; UI --- Summary
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

(prelude-require-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(prelude-require-package 'linum-relative)
(require 'linum-relative)

(prelude-require-package 'smooth-scrolling)
(require 'smooth-scrolling)
(menu-bar-mode 1)
(cua-mode 0)

(defun turn-off-guru-mode ()
  (guru-mode -1))

;;; fix annoying stuff
(add-hook 'prog-mode-hook 'turn-off-guru-mode t)
(add-hook 'prog-mode-hook 'whitespace-turn-off t)

(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)

(add-hook 'haskell-mode-hook 'turn-off-guru-mode t)
(add-hook 'haskell-mode-hook 'whitespace-turn-off t)

;;; quit wraping my lines
(setq whitespace-line-column 9999)

;;; disable bell
(setq ring-bell-function 'ignore)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(flx-ido-mode 1)

(ido-mode 1)
(ido-vertical-mode 1)

;; I like up and down arrow keys:
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; linum
;;(add-hook 'prog-mode-hook 'linum-mode)

;;(linum-mode 0)

(defun fix-linum-size ()
  (interactive)
  (set-face-attribute 'linum nil :height 110))

(add-hook 'linum-mode-hook 'fix-linum-size)

(defun linum-new-mode ()
  "If line numbers aren't displayed, then display them.
     Otherwise, toggle between absolute and relative numbers."
  (interactive)
  (if linum-mode
      (linum-relative-toggle)
    (linum-mode 1)))

(defun linum-off-mode ()
  "Toggles the line numbers as well as the fringe. This allows me
to maximize the screen estate."
  (interactive)
  (if linum-mode
      (progn
        (fringe-mode '(0 . 0))
        (linum-mode -1))

    (fringe-mode '(8 . 0))
    (linum-mode 1)))

(global-set-key (kbd "M-g l") 'linum-off-mode)  ;; For Linux
(global-set-key (kbd "M-g o") 'linum-new-mode)   ;; For Linux


(prelude-require-package 'which-key)
(require 'which-key)

(setq which-key-idle-delay 0.4)
(which-key-mode)

;; location of which-key window. valid values: top, bottom, left, right,
;; or a list of any of the two. If it's a list, which-key will always try
;; the first location first. It will go to the second location if there is
;; not enough room to display any keys in the first location
(setq which-key-side-window-location 'bottom)

;; max width of which-key window, when displayed at left or right.
;; valid values: number of columns (integer), or percentage out of current
;; frame's width (float larger than 0 and smaller than 1)
(setq which-key-side-window-max-width 0.33)

;; max height of which-key window, when displayed at top or bottom.
;; valid values: number of lines (integer), or percentage out of current
;; frame's height (float larger than 0 and smaller than 1)
(setq which-key-side-window-max-height 0.25)

(setq which-key-separator "➝" )
(setq which-key-unicode-correction 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ui.el ends here
