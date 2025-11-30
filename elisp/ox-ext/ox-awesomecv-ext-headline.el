;;; ox-awesomecv-ext-headline.el --- Extended headline handler -*- lexical-binding: t; -*-

(require 'ox-awesomecv)

(defun org-awesomecv-headline-extended (headline contents info)
  "Extended transcode HEADLINE element into awesomecv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (cons (org-element-property :CV_ENV headline)
                             (org-export-get-tags headline info)))
          (pagebreak (org-string-nw-p (org-element-property :PAGEBREAK headline))))
      (concat
       (when pagebreak "\\clearpage\n")
       (cond
        ;; is a cv entry or subentry (including our extensions)
        ((seq-intersection environment  '("cventry"
                                          "cvsubentry"
                                          "cvemployer"
                                          "cvschool"
                                          "cvhonor"
                                          "cvproject"
                                          "cvopensource"
                                          "opensourceentry"
                                          "cventryshort"
                                          "cvsubsection"
                                          "cvrole"
                                          "cvletter"
                                          "cvletter_notitle"
                                          "lettersection"
                                          "letterheader"))
         (org-awesomecv--format-cventry headline contents info))
        ((seq-intersection environment '("cventries" "cvhonors"))
         (org-awesomecv--format-cvenvironment
          (car (seq-intersection environment '("cventries" "cvhonors"))) headline contents info))
        (t (org-export-with-backend 'latex headline contents info)))))))

(provide 'ox-awesomecv-ext-headline)
;;; ox-awesomecv-ext-headline.el ends here
