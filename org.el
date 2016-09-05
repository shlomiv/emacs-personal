;;; ORG --- Summary
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
(add-to-list 'load-path "~/org-mode/lisp")
(add-to-list 'load-path "~/org-mode/contrib/lisp" t)

(prelude-require-package 'ox-reveal)
(prelude-require-package 'org-ref)

(require 'org)
(require 'ob-clojure)
(require 'ox-latex)
(require 'ox-publish)
(prelude-require-package 'org-bullets)
(require 'org-bullets)
(prelude-require-package 'htmlize)

(prelude-require-package 'gnuplot)
(require 'gnuplot)
(setq org-html-htmlize-output-type 'css)
(require 'htmlize)
(require 'ox-reveal)
(setq org-reveal-root "file:///Users/vaknins9/projects/reveal.js")

;;(require 'ox-ioslide)

(require 'ox-html)

;; hide slashes when showing italics
(setq org-hide-emphasis-markers t)

;; Taken from http://emacs.stackexchange.com/a/369/6847
;; This toggles between beginning of line (org-beginning-of-line C-a) and indentation position (back-to-indentation M-m)
(defadvice org-beginning-of-line (around ad-org-beginning-of-line-around act)
  "Move back to indentation first, then to beginning of line"
  (let ((initial-position (point)))
    ad-do-it
    (when (looking-at "^ +")
      (when (equal initial-position
                   (progn (back-to-indentation)
                          (point)))
        (beginning-of-line)))))

;; Make beutiful checkboxes with bigblow

;;;(add-to-list 'org-html-checkbox-types
;;;             '(shlomi-bigblow .
;;;                              ((on ."<span class=manual-done>[X]</span>")
;;;                               (off . "<span class=manual-not-done>[ ]</span>")
;;;                               (trans . "<span class=manual-not-done>[-]</span>"))))
;;;
;;(setq org-html-checkbox-type 'shlomi-bigblow)

(defun org-font-lock-ensure ()
  (font-lock-fontify-buffer))

;; enable babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (R . t)
   (python . t)
   (C . t)
   (clojure . t)
   (emacs-lisp . t)
   (clojure . t)
   (shell . t)
   (gnuplot . t)))


(setq org-image-actual-width 100)

(setq org-src-fontify-natively t)

(setq org-babel-clojure-backend 'cider)

(setq org-confirm-babel-evaluate 'nil)

(setq org-latex-table-caption-above 'nil)
(setq org-export-latex-table-caption-above 'nil)

(add-to-list 'org-structure-template-alist '("se" "#+BEGIN_SRC elisp :exports results\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
(add-to-list 'org-structure-template-alist '("clj" "#+BEGIN_SRC clojure ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
(add-to-list 'org-structure-template-alist '("cljo" "#+BEGIN_SRC clojure :exports code :result none\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
(add-to-list 'org-structure-template-alist '("sho" "#+BEGIN_SRC sh :exports code :result none\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
(add-to-list 'org-structure-template-alist '("t" "#+BEGIN_TIP\n?\n#+END_TIP" "<div class=\"tip\">\n\n</div>"))
(add-to-list 'org-structure-template-alist '("w" "#+BEGIN_warning\n?\n#+END_warning" "<div class=\"warning\">\n\n</div>"))
(add-to-list 'org-structure-template-alist '("n" "#+BEGIN_note\n?\n#+END_note" "<div class=\"note\">\n\n</div>"))
(add-to-list 'org-structure-template-alist '("f" "#+BEGIN_info\n?\n#+END_info" "<div class=\"info\">\n\n</div>"))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; minted export
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted" nil))
;;(setq org-latex-minted-options '(("xleftmargin" "2em") ("fontsize" "\\footnotesize")))
;; Capture stuff

;;(setq org-default-notes-file (concat org-directory "~notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org//notes.org" "Tasks")
         "* TODO %?\n  %i\n  SCHEDULED: %T\n  %a")
        ("n" "Note on current clocked item" entry (clock)
         "* Note: %?\n  %i\n  %U\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("h" "Home task" entry (file+headline "~/org/home.org" "Home")
         "* TODO  %? :tali:\n  %i\n  %U")
        ))

(setq org-agenda-custom-commands
      '(("r" tags-todo "ri")
        ("T" tags-todo "tali")
        ;;("" tags-todo "-tali-tax")
        ))

(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `org-babel-temporary-directory'."
  (if (file-remote-p default-directory)
      (let ((prefix
             ;; We cannot use `temporary-file-directory' as local part
             ;; on the remote host, because it might be another OS
             ;; there.  So we assume "/tmp", which ought to exist on
             ;; relevant architectures.
             (concat (file-remote-p default-directory)
                     ;; REPLACE temporary-file-directory with /tmp:
                     (expand-file-name prefix "/tmp/"))))
        (make-temp-file prefix nil suffix))
    (let ((temporary-file-directory
           (or (and (boundp 'org-babel-temporary-directory)
                    (file-exists-p org-babel-temporary-directory)
                    org-babel-temporary-directory)
               temporary-file-directory)))
      (make-temp-file prefix nil suffix))))

(setq reftex-default-bibliography '("~/bib.bib"))

(defun org-mode-reftex-setup1111 ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn                           ;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         (reftex-parse-all)             ;add a custom reftex cite format to insert links
         (reftex-set-cite-format "** [[papers:%l][%l]]: %t \n")))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

(setq org-link-abbrev-alist
      '(("papers" . "~/papers/%s.pdf")))


(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;;(setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))

;;(setq org-latex-pdf-process
;;      "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")

;;(setq org-latex-to-pdf-process '("xelatex %f && bibtex %f && xelatex %f && xelatex %f"))
;;(setq org-latex-to-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

;;(setq org-latex-to-pdf-process (list "latexmk -pdf %f"))


(setq org-ditaa-jar-path "~/org-mode/org-8.2.7c/contrib/scripts/ditaa.jar")

;; Before working on minted, I used this line
;;(setq org-latex-to-pdf-process (list "latexmk  -pdf -bibtex %f -shell-escape"))


(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x C-n") 'org-capture)

(require 'python)

(add-hook 'org-mime-html-hook          'inline-css)

(defun inline-css ()
  (run-python)
  (let ((new-body (python-shell-send-string-no-output
                   (format "from premailer import transform\nprint transform(\"\"\" %s \"\"\")"
                           (buffer-string))
                   (python-shell-internal-get-or-create-process))
                  ))
    (erase-buffer)
    (insert new-body)
    (message new-body)
    (beginning-of-buffer)
    (kill-line)
    ))

;; (setq org-publish-project-alist
;;       '(("spanish"
;;          :base-directory "~/work/reports/spanish/"
;;          :base-extension "org"
;;          :publishing-directory "~/public_html/"
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4             ; Just the default for this project.
;;          :auto-preamble t
;;          )
;;         ("reports"
;;          :base-directory "~/work/reports/"
;;          :base-extension "org"
;;          :publishing-directory "~/public_html/"
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4             ; Just the default for this project.
;;          :auto-preamble t
;;          )
;;         ("org-notes"
;;          :base-directory "~/org/"
;;          :base-extension "org"
;;          :publishing-directory "~/public_html/"
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4             ; Just the default for this project.
;;          :auto-preamble t
;;          )
;;         ("org-static"
;;          :base-directory "~/org/"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;          :publishing-directory "~/public_html/"
;;          :recursive t
;;          :publishing-function org-publish-attachment
;;          )
;;         ("org" :components ("org-notes" "spanish" "org-static"))))

;; My little fix which exports links across different org files...
;;(defun org-html-link (link desc info)
;;  "Transcode a LINK object from Org to HTML.
;;
;;DESC is the description part of the link, or the empty string.
;;INFO is a plist holding contextual information.  See
;;`org-export-data'."
;;  (let* ((home (when (plist-get info :html-link-home)
;;		 (org-trim (plist-get info :html-link-home))))
;;	 (use-abs-url (plist-get info :html-link-use-abs-url))
;;	 (link-org-files-as-html-maybe
;;	  (function
;;	   (lambda (raw-path info)
;;	     "Treat links to `file.org' as links to `file.html', if needed.
;;           See `org-html-link-org-files-as-html'."
;;	     (cond
;;	      ((and org-html-link-org-files-as-html
;;		    (string= ".org"
;;			     (downcase (file-name-extension raw-path "."))))
;;	       (concat (file-name-sans-extension raw-path) "."
;;		       (plist-get info :html-extension)))
;;	      (t raw-path)))))
;;	 (type (org-element-property :type link))
;;	 (raw-path (org-element-property :path link))
;;	 ;; Ensure DESC really exists, or set it to nil.
;;	 (desc (org-string-nw-p desc))
;;	 (path
;;	  (cond
;;	   ((member type '("http" "https" "ftp" "mailto"))
;;	    (org-link-escape
;;	     (org-link-unescape
;;	      (concat type ":" raw-path)) org-link-escape-chars-browser))
;;	   ((string= type "file")
;;	    ;; Treat links to ".org" files as ".html", if needed.
;;	    (setq raw-path
;;		  (funcall link-org-files-as-html-maybe raw-path info))
;;	    ;; If file path is absolute, prepend it with protocol
;;	    ;; component - "file:".
;;	    (cond
;;	     ((file-name-absolute-p raw-path)
;;	      (setq raw-path (concat "file:" raw-path)))
;;	     ((and home use-abs-url)
;;	      (setq raw-path (concat (file-name-as-directory home) raw-path))))
;;	    ;; Add search option, if any.  A search option can be
;;	    ;; relative to a custom-id or a headline title.  Any other
;;	    ;; option is ignored.
;;	    (let ((option (org-element-property :search-option link)))
;;	      (cond ((not option) raw-path)
;;		    ((eq (aref option 0) ?#) (concat raw-path option))
;;		    ;; External fuzzy link: try to resolve it if path
;;		    ;; belongs to current project, if any.
;;		    ((eq (aref option 0) ?*)
;;                     (concat
;;                      raw-path
;;                      (let ((numbers
;;                             (org-publish-resolve-external-fuzzy-link
;;                              (org-element-property :path link) option)))
;;                        (and numbers (concat "#sec-"
;;                                             (mapconcat 'number-to-string
;;                                                        numbers "-"))))))
;;		    (t (concat raw-path  "#" (org-element-property :search-option link))))))
;;	   (t raw-path)))
;;	 ;; Extract attributes from parent's paragraph.  HACK: Only do
;;	 ;; this for the first link in parent (inner image link for
;;	 ;; inline images).  This is needed as long as attributes
;;	 ;; cannot be set on a per link basis.
;;	 (attributes-plist
;;	  (let* ((parent (org-export-get-parent-element link))
;;		 (link (let ((container (org-export-get-parent link)))
;;			 (if (and (eq (org-element-type container) 'link)
;;				  (org-html-inline-image-p link info))
;;			     container
;;			   link))))
;;	    (and (eq (org-element-map parent 'link 'identity info t) link)
;;		 (org-export-read-attribute :attr_html parent))))
;;	 (attributes
;;	  (let ((attr (org-html--make-attribute-string attributes-plist)))
;;	    (if (org-string-nw-p attr) (concat " " attr) "")))
;;	 protocol)
;;    (cond
;;     ;; Image file.
;;     ((and org-html-inline-images
;;	   (org-export-inline-image-p link org-html-inline-image-rules))
;;      (org-html--format-image path attributes-plist info))
;;     ;; Radio target: Transcode target's contents and use them as
;;     ;; link's description.
;;     ((string= type "radio")
;;      (let ((destination (org-export-resolve-radio-link link info)))
;;	(if (not destination) desc
;;	  (format "<a href=\"#%s\"%s>%s</a>"
;;		  (org-export-solidify-link-text
;;		   (org-element-property :value destination))
;;		  attributes desc))))
;;     ;; Links pointing to a headline: Find destination and build
;;     ;; appropriate referencing command.
;;     ((member type '("custom-id" "fuzzy" "id"))
;;      (let ((destination (if (string= type "fuzzy")
;;			     (org-export-resolve-fuzzy-link link info)
;;			   (org-export-resolve-id-link link info))))
;;	(case (org-element-type destination)
;;	  ;; ID link points to an external file.
;;	  (plain-text
;;	   (let ((fragment (concat "ID-" path))
;;		 ;; Treat links to ".org" files as ".html", if needed.
;;		 (path (funcall link-org-files-as-html-maybe
;;				destination info)))
;;	     (format "<a href=\"%s#%s\"%s>%s</a>"
;;		     path fragment attributes (or desc destination))))
;;	  ;; Fuzzy link points nowhere.
;;	  ((nil)
;;	   (format "<i>%s</i>"
;;		   (or desc
;;		       (org-export-data
;;			(org-element-property :raw-link link) info))))
;;	  ;; Link points to a headline.
;;	  (headline
;;	   (let ((href
;;		  ;; What href to use?
;;		  (cond
;;		   ;; Case 1: Headline is linked via it's CUSTOM_ID
;;		   ;; property.  Use CUSTOM_ID.
;;		   ((string= type "custom-id")
;;		    (org-element-property :CUSTOM_ID destination))
;;		   ;; Case 2: Headline is linked via it's ID property
;;		   ;; or through other means.  Use the default href.
;;		   ((member type '("id" "fuzzy"))
;;		    (format "sec-%s"
;;			    (mapconcat 'number-to-string
;;				       (org-export-get-headline-number
;;					destination info) "-")))
;;		   (t (error "Shouldn't reach here"))))
;;		 ;; What description to use?
;;		 (desc
;;		  ;; Case 1: Headline is numbered and LINK has no
;;		  ;; description.  Display section number.
;;		  (if (and (org-export-numbered-headline-p destination info)
;;			   (not desc))
;;		      (mapconcat 'number-to-string
;;				 (org-export-get-headline-number
;;				  destination info) ".")
;;		    ;; Case 2: Either the headline is un-numbered or
;;		    ;; LINK has a custom description.  Display LINK's
;;		    ;; description or headline's title.
;;		    (or desc (org-export-data (org-element-property
;;					       :title destination) info)))))
;;	     (format "<a href=\"#%s\"%s>%s</a>"
;;		     (org-export-solidify-link-text href) attributes desc)))
;;	  ;; Fuzzy link points to a target or an element.
;;	  (t
;;	   (let* ((path (org-export-solidify-link-text path))
;;		  (org-html-standalone-image-predicate 'org-html--has-caption-p)
;;		  (number (cond
;;			   (desc nil)
;;			   ((org-html-standalone-image-p destination info)
;;			    (org-export-get-ordinal
;;			     (org-element-map destination 'link
;;			       'identity info t)
;;			     info 'link 'org-html-standalone-image-p))
;;			   (t (org-export-get-ordinal
;;			       destination info nil 'org-html--has-caption-p))))
;;		  (desc (cond (desc)
;;			      ((not number) "No description for this link")
;;			      ((numberp number) (number-to-string number))
;;			      (t (mapconcat 'number-to-string number ".")))))
;;	     (format "<a href=\"#%s\"%s>%s</a>" path attributes desc))))))
;;     ;; Coderef: replace link with the reference name or the
;;     ;; equivalent line number.
;;     ((string= type "coderef")
;;      (let ((fragment (concat "coderef-" path)))
;;	(format "<a href=\"#%s\"%s%s>%s</a>"
;;		fragment
;;		(org-trim
;;		 (format (concat "class=\"coderef\""
;;				 " onmouseover=\"CodeHighlightOn(this, '%s');\""
;;				 " onmouseout=\"CodeHighlightOff(this, '%s');\"")
;;			 fragment fragment))
;;		attributes
;;		(format (org-export-get-coderef-format path desc)
;;			(org-export-resolve-coderef path info)))))
;;     ;; Link type is handled by a special function.
;;     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
;;      (funcall protocol (org-link-unescape path) desc 'html))
;;     ;; External link with a description part.
;;     ((and path desc) (format "<a href=\"%s\"%s>%s</a>" path attributes desc))
;;     ;; External link without a description part.
;;     (path (format "<a href=\"%s\"%s>%s</a>" path attributes path))
;;     ;; No path, only description.  Try to do something useful.
;;     (t (format "<i>%s</i>" desc)))))
;;

;; Set default clocktable to use work days (8 hours per day)
(setq org-time-clocksum-format
      (quote
       (:work-days "%dd " :hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

;; Add work-day key (set for 8 hours a working day)
(defun org-minutes-to-clocksum-string (m)
  "Format number of minutes as a clocksum string.
The format is determined by `org-time-clocksum-format',
`org-time-clocksum-use-fractional' and
`org-time-clocksum-fractional-format' and
`org-time-clocksum-use-effort-durations'."
  (let ((clocksum "")
	(m (round m)) ; Don't allow fractions of minutes
	h d w mo y fmt n wd)
    (setq h (if org-time-clocksum-use-effort-durations
		(cdr (assoc "h" org-effort-durations)) 60)
	  d (if org-time-clocksum-use-effort-durations
		(/ (cdr (assoc "d" org-effort-durations)) h) 24)
	  wd (if org-time-clocksum-use-effort-durations
		(/ (cdr (assoc "wd" org-effort-durations)) h) 8)
	  w (if org-time-clocksum-use-effort-durations
		(/ (cdr (assoc "w" org-effort-durations)) (* d h)) 7)
	  mo (if org-time-clocksum-use-effort-durations
		 (/ (cdr (assoc "m" org-effort-durations)) (* d h)) 30)
	  y (if org-time-clocksum-use-effort-durations
		(/ (cdr (assoc "y" org-effort-durations)) (* d h)) 365))
    ;; fractional format
    (if org-time-clocksum-use-fractional
	(cond
	 ;; single format string
	 ((stringp org-time-clocksum-fractional-format)
	  (format org-time-clocksum-fractional-format (/ m (float h))))
	 ;; choice of fractional formats for different time units
	 ((and (setq fmt (plist-get org-time-clocksum-fractional-format :years))
	       (> (/ (truncate m) (* y d h)) 0))
	  (format fmt (/ m (* y d (float h)))))
	 ((and (setq fmt (plist-get org-time-clocksum-fractional-format :months))
	       (> (/ (truncate m) (* mo d h)) 0))
	  (format fmt (/ m (* mo d (float h)))))
	 ((and (setq fmt (plist-get org-time-clocksum-fractional-format :weeks))
	       (> (/ (truncate m) (* w d h)) 0))
	  (format fmt (/ m (* w d (float h)))))
	 ((and (setq fmt (plist-get org-time-clocksum-fractional-format :days))
	       (> (/ (truncate m) (* d h)) 0))
	  (format fmt (/ m (* d (float h)))))
         ((and (setq fmt (plist-get org-time-clocksum-fractional-format :work-days))
	       (> (/ (truncate m) (* wd h)) 0))
	  (format fmt (/ m (* d (float h)))))
	 ((and (setq fmt (plist-get org-time-clocksum-fractional-format :hours))
	       (> (/ (truncate m) h) 0))
	  (format fmt (/ m (float h))))
	 ((setq fmt (plist-get org-time-clocksum-fractional-format :minutes))
	  (format fmt m))
	 ;; fall back to smallest time unit with a format
	 ((setq fmt (plist-get org-time-clocksum-fractional-format :hours))
	  (format fmt (/ m (float h))))
	 ((setq fmt (plist-get org-time-clocksum-fractional-format :days))
	  (format fmt (/ m (* d (float h)))))
         ((setq fmt (plist-get org-time-clocksum-fractional-format :work-days))
	  (format fmt (/ m (* wd (float h)))))
	 ((setq fmt (plist-get org-time-clocksum-fractional-format :weeks))
	  (format fmt (/ m (* w d (float h)))))
	 ((setq fmt (plist-get org-time-clocksum-fractional-format :months))
	  (format fmt (/ m (* mo d (float h)))))
	 ((setq fmt (plist-get org-time-clocksum-fractional-format :years))
	  (format fmt (/ m (* y d (float h))))))
      ;; standard (non-fractional) format, with single format string
      (if (stringp org-time-clocksum-format)
	  (format org-time-clocksum-format (setq n (/ m h)) (- m (* h n)))
	;; separate formats components
	(and (setq fmt (plist-get org-time-clocksum-format :years))
	     (or (> (setq n (/ (truncate m) (* y d h))) 0)
		 (plist-get org-time-clocksum-format :require-years))
	     (setq clocksum (concat clocksum (format fmt n))
		   m (- m (* n y d h))))
	(and (setq fmt (plist-get org-time-clocksum-format :months))
	     (or (> (setq n (/ (truncate m) (* mo d h))) 0)
		 (plist-get org-time-clocksum-format :require-months))
	     (setq clocksum (concat clocksum (format fmt n))
		   m (- m (* n mo d h))))
	(and (setq fmt (plist-get org-time-clocksum-format :weeks))
	     (or (> (setq n (/ (truncate m) (* w d h))) 0)
		 (plist-get org-time-clocksum-format :require-weeks))
	     (setq clocksum (concat clocksum (format fmt n))
		   m (- m (* n w d h))))
	(and (setq fmt (plist-get org-time-clocksum-format :work-days))
	     (or (> (setq n (/ (truncate m) (* wd h))) 0)
		 (plist-get org-time-clocksum-format :require-work-days))
	     (setq clocksum (concat clocksum (format fmt n))
		   m (- m (* n wd h))))
        (and (setq fmt (plist-get org-time-clocksum-format :days))
	     (or (> (setq n (/ (truncate m) (* d h))) 0)
		 (plist-get org-time-clocksum-format :require-days))
	     (setq clocksum (concat clocksum (format fmt n))
		   m (- m (* n d h))))
	(and (setq fmt (plist-get org-time-clocksum-format :hours))
	     (or (> (setq n (/ (truncate m) h)) 0)
		 (plist-get org-time-clocksum-format :require-hours))
	     (setq clocksum (concat clocksum (format fmt n))
		   m (- m (* n h))))
	(and (setq fmt (plist-get org-time-clocksum-format :minutes))
	     (or (> m 0) (plist-get org-time-clocksum-format :require-minutes))
	     (setq clocksum (concat clocksum (format fmt m))))
	;; return formatted time duration
	clocksum))))

(setq bibtex-completion-bibliography "~/org/bibliography/references.bib"
      bibtex-completion-library-path "~/org/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/org/bibliography/helm-bibtex-notes")


;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
        (start-process "open" "*open*" "open" fpath)))
(lambda (fpath) (start-process "open" "*open*" "open" fpath))

(setq reftex-default-bibliography '("~/org/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
      org-ref-default-bibliography '("~/org/bibliography/references.bib")
      org-ref-pdf-directory "~/org/bibliography/bibtex-pdfs/")

(require 'org-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org.el ends here
