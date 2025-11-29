;;; ox-awesomecv-ext-entries.el --- Additional entry type handlers -*- lexical-binding: t; -*-

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
  (format "\\cvrole{%s}%s"
          title
          (if contents (concat "\n" contents) "")))

(provide 'ox-awesomecv-ext-entries)
;;; ox-awesomecv-ext-entries.el ends here
