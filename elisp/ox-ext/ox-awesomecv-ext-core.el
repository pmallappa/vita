;;; ox-awesomecv-ext-core.el --- Core formatting dispatcher -*- lexical-binding: t; -*-

(require 'org)
(require 'ox-awesomecv)
(require 'ox-awesomecv-ext-utils)
(require 'ox-awesomecv-ext-project)
(require 'ox-awesomecv-ext-entries)

(defun org-awesomecv--format-cventry-extended (headline contents info)
  "Extended format for HEADLINE supporting additional entry types.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((entrytype
          (cl-find-if (lambda (s) (or (string-prefix-p "cv" s)
                                      (string-prefix-p "letter" s)
                                      (string-prefix-p "opensource" s)))
                      (cons (org-element-property :CV_ENV headline)
                            (org-export-get-tags headline info))))
         (title (org-export-data (org-element-property :title headline) info))
         (date (org-element-property :DATE headline))
         (from-date (or (org-element-property :FROM headline) date))
         (to-date (or (org-element-property :TO headline) date))
         (employer (or (org-element-property :ORGANIZATION headline)
                       (org-element-property :SCHOOL headline)
                       (org-element-property :EMPLOYER headline)
                       (org-element-property :EVENT headline)
                       (org-element-property :PROJECT headline)
                       (org-element-property :POSITION headline) ""))
         (location (or (org-element-property :LOCATION headline) "")))
    
    (cond
     ;; Handle cvemployer (company header in experience/projects)
     ((string= entrytype "cvemployer")
      (org-awesomecv-ext--format-cvemployer headline contents info from-date to-date employer location title))
     
     ;; Handle cvproject
     ((string= entrytype "cvproject")
      (org-awesomecv-ext--format-cvproject headline contents info from-date to-date))
     
     ;; Handle cvopensource (deprecated - use opensourceentry)
     ((string= entrytype "cvopensource")
      (org-awesomecv-ext--format-cvopensource headline contents info from-date to-date))
     
     ;; Handle opensourceentry
     ((string= entrytype "opensourceentry")
      (org-awesomecv-ext--format-opensourceentry headline contents info from-date to-date))
     
     ;; Handle cventryshort
     ((string= entrytype "cventryshort")
      (org-awesomecv-ext--format-cventryshort headline contents info from-date to-date employer location title))
     
     ;; Handle cvsubsection
     ((string= entrytype "cvsubsection")
      (org-awesomecv-ext--format-cvsubsection headline contents title))
     
     ;; Handle cvrole
     ((string= entrytype "cvrole")
      (org-awesomecv-ext--format-cvrole headline contents title))

      ;; Handle cvhonor to ensure 5th argument is present
      ((string= entrytype "cvhonor")
       (org-awesomecv-ext--format-cvhonor headline contents info from-date to-date employer location title))
     
     ;; For all other types, fall back to original function
     (t nil))))

(defun org-awesomecv--format-cventry-with-extensions (orig-fun headline contents info)
  "Wrapper to add extension support to cventry formatting.
ORIG-FUN is the original formatting function.
HEADLINE, CONTENTS, and INFO are standard org-export parameters."
  (or (org-awesomecv--format-cventry-extended headline contents info)
      (funcall orig-fun headline contents info)))

(defun org-awesomecv-ext--plain-list-cvrole-p (plain-list info)
  "Return non-nil when PLAIN-LIST belongs to a cvrole section."
  (let* ((cv-env (org-entry-get (org-element-property :begin plain-list) "CV_ENV" nil))
         (tags (org-export-get-tags plain-list info nil 'inherited))
         (parent-type (car (org-element-property :parent plain-list))))
    (and (eq parent-type 'section)
         (or (string= cv-env "cvrole")
             (member "cvrole" tags)))))

(defun org-awesomecv-ext--plain-list-advice (orig-fun plain-list contents info)
  "Wrap cvrole lists with cvitems using ORIG-FUN fallback."
  (if (org-awesomecv-ext--plain-list-cvrole-p plain-list info)
      (format "\\begin{cvitems}\n%s\\end{cvitems}" contents)
    (funcall orig-fun plain-list contents info)))

(advice-add 'org-awesomecv-plain-list :around #'org-awesomecv-ext--plain-list-advice)
(advice-add 'org-awesomecv2-plain-list :around #'org-awesomecv-ext--plain-list-advice)

(provide 'ox-awesomecv-ext-core)
;;; ox-awesomecv-ext-core.el ends here
