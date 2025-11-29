;;; debug-cv.el --- Helpers to inspect CV Org data -*- lexical-binding: t; -*-

(require 'org)

(defconst org-debug--elisp-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the CV-specific Emacs helpers.")

(add-to-list 'load-path org-debug--elisp-dir)
(require 'org-cv-init)

(setq debug-on-error t)

(find-file (expand-file-name "prem-mallappa-vita.org" org-cv--org-root))
(org-map-entries
 (lambda ()
   (let* ((env (org-entry-get nil "CV_ENV"))
          (needs-date (member env '("cvschool" "cvemployer" "cvsubentry" "cventry" "cvhonor")))
          (title (org-get-heading t t t t))
          (from (org-entry-get nil "FROM"))
          (to (org-entry-get nil "TO"))
          (date (org-entry-get nil "DATE")))
     (when env
       (message "ENV=%s TITLE=%s FROM=%s TO=%s DATE=%s" env title from to date))
     (when (and needs-date (not (or from date)))
       (message "MISSING DATE -> %s (env=%s)" title env))))
 t 'file)
(kill-emacs)

;;; debug-cv.el ends here
