;;; ox-awesomecv-ext-utils.el --- Utility functions for ox-awesomecv extensions -*- lexical-binding: t; -*-

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

(provide 'ox-awesomecv-ext-utils)
;;; ox-awesomecv-ext-utils.el ends here
