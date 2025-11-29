;;; ox-awesomecv-ext-core.el --- Core formatting dispatcher -*- lexical-binding: t; -*-

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
                                      (string-prefix-p "letter" s)))
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
     ;; Handle cvproject
     ((string= entrytype "cvproject")
      (org-awesomecv-ext--format-cvproject headline contents info from-date to-date))
     
     ;; Handle cvopensource
     ((string= entrytype "cvopensource")
      (org-awesomecv-ext--format-cvopensource headline contents info from-date to-date))
     
     ;; Handle cventryshort
     ((string= entrytype "cventryshort")
      (org-awesomecv-ext--format-cventryshort headline contents info from-date to-date employer location title))
     
     ;; Handle cvsubsection
     ((string= entrytype "cvsubsection")
      (org-awesomecv-ext--format-cvsubsection headline contents title))
     
     ;; Handle cvrole
     ((string= entrytype "cvrole")
      (org-awesomecv-ext--format-cvrole headline contents title))
     
     ;; For all other types, fall back to original function
     (t nil))))

(defun org-awesomecv--format-cventry-with-extensions (orig-fun headline contents info)
  "Wrapper to add extension support to cventry formatting.
ORIG-FUN is the original formatting function.
HEADLINE, CONTENTS, and INFO are standard org-export parameters."
  (or (org-awesomecv--format-cventry-extended headline contents info)
      (funcall orig-fun headline contents info)))

(provide 'ox-awesomecv-ext-core)
;;; ox-awesomecv-ext-core.el ends here
