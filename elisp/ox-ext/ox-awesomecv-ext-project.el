;;; ox-awesomecv-ext-project.el --- Project entry handlers -*- lexical-binding: t; -*-

(require 'ox-awesomecv-ext-utils)

(defun org-awesomecv-ext--format-cvproject (headline contents info from-date to-date)
  "Format cvproject entry for HEADLINE.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE and TO-DATE specify the time range."
  (let* ((url (org-element-property :URL headline))
         (link-prop (org-element-property :LINK headline))
         (project-link (or url link-prop))
         ;; Extract plain text title without hyperlink
         (plain-title (org-element-interpret-data (org-element-property :title headline)))
         ;; Build margin notes
         (marginnote (org-awesomecv-ext--build-marginnote 
                      headline 
                      org-awesomecv-ext--margin-props 
                      project-link)))
    (format "\\projectsubentry{%s}{%s}{%s}{%s}"
            plain-title
            (org-cv-utils--format-time-window from-date to-date)
            contents
            (if (string-empty-p marginnote) "" marginnote))))

(defun org-awesomecv-ext--format-cvopensource (headline contents info from-date to-date)
  "Format cvopensource entry for HEADLINE.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE and TO-DATE specify the time range."
  (let* ((url (org-element-property :URL headline))
         (link-prop (org-element-property :LINK headline))
         (project-link (or url link-prop))
         ;; Extract plain text title without hyperlink
         (plain-title (org-element-interpret-data (org-element-property :title headline)))
         ;; Build margin notes
         (marginnote (org-awesomecv-ext--build-marginnote 
                      headline 
                      org-awesomecv-ext--margin-props 
                      project-link)))
    (format "\\opensourcesubentry{%s}{%s}{%s}{%s}"
            plain-title
            (org-cv-utils--format-time-window from-date to-date)
            contents
            (if (string-empty-p marginnote) "" marginnote))))

(provide 'ox-awesomecv-ext-project)
;;; ox-awesomecv-ext-project.el ends here
