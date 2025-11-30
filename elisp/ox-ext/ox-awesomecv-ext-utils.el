;;; ox-awesomecv-ext-utils.el --- Utility functions for ox-awesomecv extensions -*- lexical-binding: t; -*-

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
   ((and fallback (string-match "\\`\\s-*\\([^\\-]+?\\)\\s*--\\s*\\(.+\\)\\'" fallback))
    (cons (string-trim (match-string 1 fallback))
          (string-trim (match-string 2 fallback))))
   ((and fallback (string-match "\\`\\s-*\\([^\\-]+?\\)\\s*-\\s*\\(.+\\)\\'" fallback))
    (cons (string-trim (match-string 1 fallback))
          (string-trim (match-string 2 fallback))))
   (fallback
    (cons (string-trim fallback) ""))
   (t (cons "" ""))))

(defun org-awesomecv-ext--render-marginnote (note)
  "Render NOTE inside a formatted LaTeX margin note if content exists."
  (when (org-string-nw-p (org-trim note))
    (format "\\marginnote{\\raggedright %s}[-0.5em]\n" note)))

(provide 'ox-awesomecv-ext-utils)
;;; ox-awesomecv-ext-utils.el ends here
