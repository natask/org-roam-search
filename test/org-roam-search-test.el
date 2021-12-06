;;; org-roam-search-test.el --- Tests for org-roam-search -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: December 05, 2021
;; Modified: December 05, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/natask/org-roam-search
;; Package-Requires: (buttercup)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'buttercup)
(require 'org-roam-search)

(describe "org-roam-search--query-string-to-sexp"
  (it "handles empty string"
    (expect (org-roam-search--query-string-to-sexp "")
            :to-equal
            nil))
  (it "handles default string"
    (expect (org-roam-search--query-string-to-sexp "hello")
            :to-equal
            `(,org-roam-search-default-predicate "hello")))
  (it "random test"
    (expect (org-roam-search--query-string-to-sexp "dest:dfasdf,where dest:now,better tag:here l:3 title:there what now \"here\"")
            :to-equal
            `(and (dest "dfasdf" "where") (dest "now" "better") (tag "here") (l "3") (title "there") (all "what") (all "now") (all "here")))))

(describe "org-roam-search--transform-query"
  (let ((sexp (org-roam-search--query-string-to-sexp "title:where tag:love"))
        (res `(and (or (and (like title "%%where%%")) (and (like aliases '"%%where%%"))) (like tags '"%%love%%"))))
    (it "handles simple query"
      (expect (org-roam-search--transform-query sexp)
              :to-equal
              res
              )))
  (let ((sexp (org-roam-search--query-string-to-sexp "not where and love title:fast or context:better"))
        (res `(and (or (and (like tags '"%%not%%")) (and (like filetitle '"%%not%%")) (and (like olp '"%%not%%")) (and (like title "%%not%%")) (and (like aliases '"%%not%%"))) (or (and (like tags '"%%where%%")) (and (like filetitle '"%%where%%")) (and (like olp '"%%where%%")) (and (like title "%%where%%")) (and (like aliases '"%%where%%"))) (or (and (like tags '"%%and%%")) (and (like filetitle '"%%and%%")) (and (like olp '"%%and%%")) (and (like title "%%and%%")) (and (like aliases '"%%and%%"))) (or (and (like tags '"%%love%%")) (and (like filetitle '"%%love%%")) (and (like olp '"%%love%%")) (and (like title "%%love%%")) (and (like aliases '"%%love%%"))) (or (and (like title "%%fast%%")) (and (like aliases '"%%fast%%"))) (or (and (like tags '"%%or%%")) (and (like filetitle '"%%or%%")) (and (like olp '"%%or%%")) (and (like title "%%or%%")) (and (like aliases '"%%or%%"))) (or (and (like tags '"%%better%%")) (and (like filetitle '"%%better%%")) (and (like olp '"%%better%%"))))))
    (it "handles complex query with boolean operations"
      (expect (org-roam-search--transform-query sexp)
              :to-equal
              res
              )))
  (let ((sexp1 (org-roam-search--query-string-to-sexp "where love dest:love source:here"))
        (sexp2 (org-roam-search--query-string-to-sexp "where love")))
    (it "ignores dest: and source:"
      (expect (org-roam-search--transform-query sexp1)
              :to-equal
              (org-roam-search--transform-query sexp2)))))

(describe "org-roam-search--transform-destination-query"
  (let ((sexp (org-roam-search--query-string-to-sexp "where dest:hddd,3ds-dfasd3-f"))
        (res '(and (like dest "%%hddd%%") (like dest "%%3ds-dfasd3-f%%"))))
    (it "handles ids"
      (expect (org-roam-search--transform-destination-query sexp)
              :to-equal
              res
              )))

  (let ((sexp1 (org-roam-search--query-string-to-sexp "where source:best,here tag:love dest:hddd,3ds-dfasd3-f"))
        (sexp2 (org-roam-search--query-string-to-sexp "dest:hddd,3ds-dfasd3-f")))
    (it "ignores all but dest: type search"
      (expect (org-roam-search--transform-destination-query sexp1)
              :to-equal
              (org-roam-search--transform-destination-query sexp2)
              )))
  ;; complex: or dest:hello and better well here


  ;; not implemented
  ;; (let ((sexp (org-roam-search--query-string-to-sexp "dest:hddd,//google.com,3ds-dfasd3-f"))
  ;;       (res '(and (like dest "%%hddd%%") (like dest "%%3ds-dfasd3-f%%"))))
  ;;   (it "handles links through ref"
  ;;     (expect (org-roam-search--transform-destination-query sexp)
  ;;             :to-equal
  ;;             res
  ;;             )))
  )

(describe "org-roam-search--transform-source-query"
  (let ((sexp (org-roam-search--query-string-to-sexp "source:hddd,3ds-dfasd3-f"))
        (res  '(and (like source "%%hddd%%") (like source "%%3ds-dfasd3-f%%"))))
    (it "handles id"
      (expect (org-roam-search--transform-source-query sexp)
              :to-equal
              res
              )))

  (let ((sexp1 (org-roam-search--query-string-to-sexp "where source:best,here tag:love dest:hddd,3ds-dfasd3-f"))
        (sexp2 (org-roam-search--query-string-to-sexp "source:best,here")))
    (it "ignores all but source: type search"
      (expect (org-roam-search--transform-source-query sexp1)
              :to-equal
              (org-roam-search--transform-source-query sexp2)
              )))

  ;; not implemented
  ;; (let ((sexp (org-roam-search--query-string-to-sexp "source:hddd,//google.com,3ds-dfasd3-f")))
  ;;   (it "handles links through refs"
  ;;     (expect (org-roam-search--transform-source-query sexp)
  ;;             :to-equal
  ;;             nil
  ;;             )))
  )

(describe "org-roam-search--join-vecs"
  (it "handles nil"
    (expect (org-roam-search--join-vecs
             nil
             nil
             nil)
            :to-equal
            nil))

  (it "one entry"
    (let ((arg1 [:select 10
                 :where 5]))
      (expect (org-roam-search--join-vecs
               nil
               arg1
               nil
               nil)
              :to-equal
              arg1)))

  (it "multiple entries"
    (let ((arg1 [:select 10
                 :where 5])
          (arg2 [:select 15
                 :where 4])
          (arg3 [:select 40
                 :where 10])
          (res  [:select 10
                 :where 5
                 :union
                 :select 15
                 :where 4
                 :union
                 :select 40
                 :where 10]))
      (expect (org-roam-search--join-vecs
               nil
               arg1
               arg2
               nil
               arg3
               nil)
              :to-equal
              res))))

(provide 'org-roam-search-test)
;;; org-roam-search-test.el ends here
