;;; ox-awesomecv-ext-project.el --- Project entry handlers -*- lexical-binding: t; -*-

(require 'ox-awesomecv-ext-utils)

(defun org-awesomecv-ext--format-cvproject (headline contents info from-date to-date)
  "Format cvproject entry for HEADLINE.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE and TO-DATE specify the time range."
  (let* ((url (org-element-property :URL headline))
         (link-prop (org-element-property :LINK headline))
         (project-link (or url link-prop))
         (plain-title (org-element-interpret-data (org-element-property :title headline)))
         (date-prop (org-element-property :DATE headline))
         (date-pair (org-awesomecv-ext--split-date-range from-date to-date date-prop))
         (marginnote (org-awesomecv-ext--build-marginnote 
                      headline 
                      org-awesomecv-ext--margin-props 
                      project-link)))
    (format "\\projectsubentry{%s}{%s}{%s}{%s}"
            plain-title
            (org-cv-utils--format-time-window (car date-pair) (cdr date-pair))
            contents
            (if (string-empty-p marginnote) "" marginnote))))

(defun org-awesomecv-ext--format-cvopensource (headline contents info from-date to-date)
  "Format cvopensource entry for HEADLINE (deprecated - use opensourceentry).
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE and TO-DATE specify the time range."
  (let* ((url (org-element-property :URL headline))
         (link-prop (org-element-property :LINK headline))
         (project-link (or url link-prop))
         (plain-title (org-element-interpret-data (org-element-property :title headline)))
         (date-prop (org-element-property :DATE headline))
         (date-pair (org-awesomecv-ext--split-date-range from-date to-date date-prop))
         (marginnote (org-awesomecv-ext--build-marginnote 
                      headline 
                      org-awesomecv-ext--margin-props 
                      project-link)))
    (format "\\opensourcesubentry{%s}{%s}{%s}{%s}"
            plain-title
            (org-cv-utils--format-time-window (car date-pair) (cdr date-pair))
            contents
            (if (string-empty-p marginnote) "" marginnote))))

(defun org-awesomecv-ext--format-opensourceentry (headline contents info from-date to-date)
  "Format opensourceentry for HEADLINE using new opensourceproject macro.
CONTENTS holds the contents.  INFO is a plist.
FROM-DATE and TO-DATE specify the time range."
  (let* ((url (org-element-property :URL headline))
         (plain-title (org-element-interpret-data (org-element-property :title headline)))
         (date-prop (org-element-property :DATE headline))
   (date-pair (org-awesomecv-ext--split-date-range from-date to-date date-prop))
   (content-body (or contents ""))
   (has-content (not (string-blank-p (string-trim content-body)))))
    ;; \opensourceproject{name}{start}{end}{url} then content becomes bullets
    (concat
     (format "\\opensourceproject{%s}{%s}{%s}{%s}\n"
             plain-title
             (car date-pair)
             (cdr date-pair)
       (or url ""))
     (if has-content
   (format "\\begin{opensourcecontent}\n%s\\end{opensourcecontent}\n"
     content-body)
       ""))))

(provide 'ox-awesomecv-ext-project)
;;; ox-awesomecv-ext-project.el ends here
