;;; org-roam-search.el --- Search through org roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: August 30, 2021
;; Modified: August 30, 2021
;; Version: 0.0.1
;; Keywords: org-roam
;; Homepage: https://github.com/savnkk/org-roam-search
;; Package-Requires: ((emacs "26.1") (org-roam "1.2.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Search through org roam
;;
;;; Requirements:
(require 'org-roam)
(require 'sexp-string)

;;; Vars:
(defvar org-roam-search-predicates
  '((or  :name or)
    (and :name and)
    (titles :name titles :aliases '(title))
    (tags :name tags :aliases '(tag))
    (both :name both)))

(defvar org-roam-search-default-predicate-boolean 'and)
(defvar org-roam-search-default-predicate 'both)

;;; Code:
(declare-function org-roam-search--query-string-to-sexp "ext:org-roam-search" (query) t)
(fset 'org-roam-search--query-string-to-sexp
        (sexp-string--define-query-string-to-sexp-fn  "org-roam-search"))

(provide 'org-roam-search)
;;; org-roam-search.el ends here
