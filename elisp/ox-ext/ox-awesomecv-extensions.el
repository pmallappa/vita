;;; ox-awesomecv-extensions.el --- Extensions for ox-awesomecv -*- lexical-binding: t; -*-

;; This file extends ox-awesomecv with support for additional CV_ENV types
;; that are specific to the pawesome-cv class
;;
;; This is the main entry point that loads all extension modules

(require 'ox-awesomecv)
(require 'ox-awesomecv-ext-utils)
(require 'ox-awesomecv-ext-project)
(require 'ox-awesomecv-ext-entries)
(require 'ox-awesomecv-ext-core)
(require 'ox-awesomecv-ext-headline)

;; Install advice to override the formatting functions
(advice-add 'org-awesomecv--format-cventry :around #'org-awesomecv--format-cventry-with-extensions)
(advice-add 'org-awesomecv-headline :override #'org-awesomecv-headline-extended)

(provide 'ox-awesomecv-extensions)
;;; ox-awesomecv-extensions.el ends here
