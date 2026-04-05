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
         (date-pair (org-awesomecv-ext--split-date-range from-date to-date nil))
         (macro (if (org-awesomecv-ext--projects-section-p headline)
                    "\\projectcompany"
                  "\\experienceemployer")))
    (format "%s{%s}{%s}{%s}{%s}\n%s"
            macro
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

(defun org-awesomecv-ext--format-cvschool (headline contents info from-date to-date employer location title)
  "Format cvschool entry for HEADLINE using the custom education macro.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE, TO-DATE, EMPLOYER, LOCATION, and TITLE are entry details."
  (let ((affiliated (or (org-element-property :AFFILIATED headline) "")))
    (format "\\educationentry{%s}{%s}{%s}{%s}{%s}{%s}"
            title
            location
            employer
            (org-cv-utils--format-time-window from-date to-date)
            affiliated
            (if contents (org-trim contents) ""))))

(defun org-awesomecv-ext--format-cvrole (headline contents info title)
  "Format cvrole entry for HEADLINE.
CONTENTS holds the contents.  INFO is a plist.  TITLE is the role title."
  (let* ((location (org-awesomecv-ext--inherited-location headline))
         (role-macro (if (org-awesomecv-ext--projects-section-p headline)
                         (if (org-string-nw-p location)
                             (format "\\projectrole[%s]{%s}" location title)
                           (format "\\projectrole{%s}" title))
                       (format "\\experiencerole{%s}" title))))
    (format "%s\n\\begin{experiencecontent}\n%s\\end{experiencecontent}\n"
            role-macro
            (if contents (org-trim contents) ""))))

(provide 'ox-awesomecv-ext-entries)
;;; ox-awesomecv-ext-entries.el ends here
