;;; ox-awesomecv-ext-utils.el --- Utility functions for ox-awesomecv extensions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'subr-x)

;; Utility function to build margin notes from properties
(defun org-awesomecv-ext--build-marginnote (headline margin-props project-link)
  "Build margin note string from HEADLINE properties.
MARGIN-PROPS is an alist of (PROP-KEY . LABEL) pairs.
PROJECT-LINK is optional URL for the project."
  (let ((note-lines
         (delq nil  ; Remove nil entries
               (mapcar
                (lambda (prop-pair)
                  (let* ((prop-key (intern (concat ":" (car prop-pair))))
                         (prop-label (cdr prop-pair))
                         (prop-value (org-element-property prop-key headline)))
                    (cond
                     ;; Special handling for LINK/URL
                     ((and (string= (car prop-pair) "LINK") project-link)
                      (format "\\marginnotelabel{%s:} \\marginnotevalue{\\href{%s}{Project}}"
                              prop-label project-link))
                     ;; Regular properties
                     ((and prop-value (not (string= (car prop-pair) "LINK")))
                      (format "\\marginnotelabel{%s:} \\marginnotevalue{%s}"
                              prop-label prop-value))
                     ;; Return nil if property doesn't exist
                     (t nil))))
                margin-props))))
    (mapconcat 'identity note-lines "\\\\[0.2mm]\n")))

;; Define standard margin note properties (order matters for display)
(defconst org-awesomecv-ext--margin-props
  '(("LINK" . "Link")
    ("LANGUAGES" . "Languages")
    ("TOOLS" . "Tools")
    ("TECH" . "Tech")
    ("STACK" . "Stack")
    ("ARCH" . "Architecture")
    ("OS" . "OS")
    ("KEYWORDS" . "Keywords")
    ("PLATFORM" . "Platform")
    ("FRAMEWORK" . "Framework"))
  "Standard properties to include in margin notes.")

(defun org-awesomecv-ext--split-date-range (from-date to-date fallback)
  "Return a cons cell of normalized FROM and TO strings.
When FROM-DATE and TO-DATE are nil, FALLBACK is parsed for a
range separated by `--' or `-'."
  (cond
   ((or from-date to-date)
    (let* ((from (or (and from-date (string-trim from-date)) ""))
           (to (or (and to-date (string-trim to-date)) "")))
      (cond
       ((and (org-string-nw-p fallback) (string= from to))
        (org-awesomecv-ext--split-date-range nil nil fallback))
       ((and (string-empty-p to) (org-string-nw-p fallback))
        (org-awesomecv-ext--split-date-range nil nil fallback))
       ((and (string-empty-p to) (org-string-nw-p from))
        (org-awesomecv-ext--split-date-range nil nil from))
       (t (cons from to)))))
   ((org-string-nw-p fallback)
    (let* ((value (string-trim fallback))
           (parts (cond
                   ((string-match-p "\\s-+--\\s-+" value)
                    (split-string value "\\s-*--\\s-*" t))
                   ((string-match-p "\\`[^[:space:]]+-[^[:space:]]+\\'" value)
                    (split-string value "\\s*-\\s*" t))
                   (t nil))))
      (if (and parts (= (length parts) 2))
          (cons (string-trim (car parts))
                (string-trim (cadr parts)))
        (cons value ""))))
   (t (cons "" ""))))

(defconst org-awesomecv-ext--sidebar-pagebreak-command
  "\\acvsidebarpagebreak\n"
  "LaTeX emitted for top-level section breaks that switch to sidebar layout.")

(defun org-awesomecv-ext--pagebreak-command (pagebreak)
  "Return the LaTeX command for PAGEBREAK.
When PAGEBREAK is `sidebar', switch to the wide-right-margin layout before the
page break.  Any other non-empty value keeps the legacy clearpage behavior."
  (if (string= (downcase (string-trim pagebreak)) "sidebar")
      org-awesomecv-ext--sidebar-pagebreak-command
    "\\clearpage\n"))

(defun org-awesomecv-ext--render-marginnote (note)
  "Render NOTE inside a formatted LaTeX margin note if content exists."
  (when (org-string-nw-p (org-trim note))
    (format "\\marginnote{\\raggedright %s}[-0.5em]\n" note)))

(defun org-awesomecv-ext--headline-in-section-p (headline section-title)
  "Return non-nil when HEADLINE appears under SECTION-TITLE."
  (let ((node headline)
        found)
    (while (and node (not found))
      (when (and (eq (org-element-type node) 'headline)
                 (string= (org-element-property :raw-value node) section-title))
        (setq found t))
      (setq node (org-element-property :parent node)))
    found))

(defun org-awesomecv-ext--projects-section-p (headline)
  "Return non-nil when HEADLINE belongs to the Projects section."
  (org-awesomecv-ext--headline-in-section-p headline "Projects"))

(defun org-awesomecv-ext--nearest-ancestor (headline predicate)
  "Return the nearest ancestor headline for HEADLINE matching PREDICATE."
  (let ((node (org-element-property :parent headline))
        found)
    (while (and node (not found))
      (when (and (eq (org-element-type node) 'headline)
                 (funcall predicate node))
        (setq found node))
      (setq node (org-element-property :parent node)))
    found))

(defun org-awesomecv-ext--inherited-location (headline)
  "Return LOCATION from HEADLINE or the nearest enclosing cvemployer."
  (or (org-element-property :LOCATION headline)
      (let ((employer
             (org-awesomecv-ext--nearest-ancestor
              headline
              (lambda (node)
                (string= (org-element-property :CV_ENV node) "cvemployer")))))
        (and employer (org-element-property :LOCATION employer)))
      ""))

(provide 'ox-awesomecv-ext-utils)
;;; ox-awesomecv-ext-utils.el ends here
