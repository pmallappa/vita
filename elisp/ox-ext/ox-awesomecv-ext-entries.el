;;; ox-awesomecv-ext-entries.el --- Additional entry type handlers -*- lexical-binding: t; -*-

(require 'ox-awesomecv-ext-utils)

(defun org-awesomecv-ext--format-cvhonor (headline contents info from-date to-date employer location title)
  "Format cvhonor entry, ensuring description argument is present."
  (let ((date-window (org-cv-utils--format-time-window from-date to-date)))
    (format "\\cvhonor{%s}{%s}{%s}{%s}{%s}"
            title
            employer
            location
            date-window
            (if contents (org-trim contents) ""))))

(defun org-awesomecv-ext--format-cvemployer (headline contents info from-date to-date employer location title)
  "Format cvemployer (company header) for HEADLINE.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE, TO-DATE, EMPLOYER, LOCATION, and TITLE are entry details."
  (let* ((raw-title (org-element-property :raw-value headline))
         (company (or (org-string-nw-p employer)
                      (org-string-nw-p title)
                      (org-string-nw-p raw-title)
                      ""))
         (date-pair (org-awesomecv-ext--split-date-range from-date to-date nil)))
    (format "\\experienceemployer{%s}{%s}{%s}{%s}\n%s"
            company
            (car date-pair)
            (cdr date-pair)
            location
            (if contents contents ""))))

(defun org-awesomecv-ext--format-cventryshort (headline contents info from-date to-date employer location title)
  "Format cventryshort entry for HEADLINE.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE, TO-DATE, EMPLOYER, LOCATION, and TITLE are entry details."
  (format "\\shortstintentry{%s}{%s}{%s}{%s}{%s}"
          employer
          location
          title
          (org-cv-utils--format-time-window from-date to-date)
          contents))

(defun org-awesomecv-ext--format-cvsubsection (headline contents title)
  "Format cvsubsection entry for HEADLINE.
CONTENTS holds the contents.  TITLE is the subsection title."
  (format "\\cvsubsection{%s}%s"
          title
          (if contents (concat "\n" contents) "")))

(defun org-awesomecv-ext--format-cvrole (headline contents title)
  "Format cvrole entry for HEADLINE.
CONTENTS holds the contents.  TITLE is the role title."
  (format "\\experiencerole{%s}\n\\begin{experiencecontent}\n%s\\end{experiencecontent}\n"
          title
          (if contents (org-trim contents) "")))

(provide 'ox-awesomecv-ext-entries)
;;; ox-awesomecv-ext-entries.el ends here
