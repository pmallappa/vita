;;; org-cv-init.el --- Local org-cv helpers -*- lexical-binding: t; -*-

(defconst org-cv--elisp-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory that holds the org-specific Emacs helpers.")
(defconst org-cv--repo-root (expand-file-name ".." org-cv--elisp-dir)
  "Root of the repository.")

(add-to-list 'load-path (expand-file-name "org-cv" org-cv--elisp-dir))
(add-to-list 'load-path (expand-file-name "ox-ext" org-cv--elisp-dir))
(add-to-list 'load-path org-cv--elisp-dir)

(require 'ox-awesomecv)
(require 'ox-awesomecv2)
(require 'ox-awesomecv-extensions)

(add-to-list 'org-latex-classes
             '("pawesome-cv"
               "\\documentclass{pawesome-cv}\n"
               ("\\cvsection{%s}" . "\\cvsection{%s}")
               ("\\cvsubsection{%s}" . "\\cvsubsection{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\cvparagraph{%s}" . "\\cvparagraph{%s}")))

(provide 'org-cv-init)
;;; org-cv-init.el ends here
