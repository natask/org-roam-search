;;; org-roam-search.el --- Search through org roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: August 30, 2021
;; Modified: August 30, 2021
;; Version: 0.0.1
;; Keywords: matching,org-roam
;; Homepage: https://github.com/savnkk/org-roam-search
;; Package-Requires: ((emacs "26.1") (org-roam "1.2.3") (sexp-string "0.0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Search through org roam
;;
;;; Requirements:
(require 'org-roam)
(require 'cl-lib)
(require 'sexp-string)
(require 'delve-show)
(require 'helm)

;;; Vars:
(defvar org-roam-search-predicates
  '((or  :name or  :transform
         ((`(or . ,clauses) `(or ,@(mapcar #' rec clauses))))
         :stringify
         ((`(or . ,clauses) (cl-reduce (lambda (acc elem)
                                         (let ((res (rec elem)))
                                           ;; HACK: works because plist-get grabs the first it can find.
                                           ;; Here it grabs the most recently computed value.
                                           ;; This means need to reverse
                                           ;; while presevering order of subsequent Even and Odd
                                           ;; index elements
                                           (-concat res acc)))
                                       clauses :initial-value accum))))
    (not :name not :transform
         ((`(not . ,clauses) `(not ,@(mapcar #' rec clauses)))))
    (and :name and
         :transform
         ((`(and . ,clauses) `(and ,@(mapcar #' rec clauses))))
         :stringify
         ((`(and . ,clauses) (cl-reduce (lambda (acc elem)
                                          (let ((res (rec elem)))
                                            (list :title (-concat (plist-get acc :title)
                                                                  (plist-get res :title))
                                                  :aliases (-concat (plist-get acc :aliases)
                                                                    (plist-get res :aliases))
                                                  :tags (-concat (plist-get acc :tags)
                                                                 (plist-get res :tags)))))
                                        clauses :initial-value accum))))
    (titles :name titles :aliases '(title aliases alias)
            :transform
            ((`(,(or 'titles 'title 'aliases 'alias) . ,rest)
              (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like titles:title ,(rec elem)))) (cons 'and rest))))
            :stringify
            ((`(,(or 'titles 'title 'aliases 'alias) . ,rest)
              (plist-put accum :aliases (append (plist-get accum :aliases) rest)))))
    (tags :name tags :aliases '(tag)
          :transform
          ((`(,(or 'tags 'tag) . ,rest)
            (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like tags:tags ,(rec elem)))) (cons 'and rest))))
          :stringify
          ((`(,(or 'tags 'tag) . ,rest)
            (plist-put accum :tags (-concat (plist-get accum :tags) rest)))))
    (both :name both
          :transform
          ((`(both . ,rest)
            (rec `(or (tags ,@rest)
                      (titles ,@rest)))))
          :search
          ((`(both . ,rest)
            (list :title (plist-get (rec `(titles ,@rest) accum) :title)
                  :tags (plist-get (rec `(tags ,@rest) accum) :tags))))
          :stringify
          ((`(both . ,rest)
            (plist-put accum :title (-concat (plist-get accum :title) rest)))))
    (query :name query
           :transform
           (((pred stringp) `',(concat "%%" element "%%")))
           :search
           (((pred stringp) element))
           :stringify
           (((pred stringp) element))))
  "Predicate list to convert string to sexp.")

(defvar org-roam-search-default-boolean 'and
  "Predicate default binary function.")
(defvar org-roam-search-default-predicate 'both
  "Predicate default type.")
(defvar org-roam-search-pexs-function 'sexp-string--custom-pexs
  "Pex parsing macro.")
(defvar org-roam-search-prefix-index 0
  "Index to insert user input within candidates.")
(defvar org-roam-search-history 'nil
  "List of prior searched items.")
(defvar-local org-roam-search--delve-local-query 'nil
  "Local variable holding query.
generated from delve enteries from org-roam completion.")
(defvar org-roam-search--kill-buffers-list 'nil
  "List of buffers to kill when completion framework exits.
buffers opened using persistent-action.")
(defvar org-roam-search-default-tags 'nil
  "List of tags to add to each file created")

;;; Code:
(declare-function org-roam-search--query-string-to-sexp "ext:org-roam-search" (query) t)
(declare-function org-roam-search--transform-query "ext:org-roam-search" (query) t)
(declare-function org-roam-search--stringify-query "ext:org-roam-search" (query) t)
(fset 'org-roam-search--query-string-to-sexp
      (sexp-string--define-query-string-to-sexp-fn  "org-roam-search"))
(fset 'org-roam-search--transform-query (sexp-string--define-transform-query-fn "org-roam-search" :transform))
(fset 'org-roam-search--stringify-query (sexp-string--define-transform-query-fn "org-roam-search" :stringify))

(defun org-roam-search--get-title-path-completions (&optional conditions)
  "Return an alist for completion on CONDITIONS.
The car is the displayed title for completion, and the cdr is a
plist containing the path and title for the file."
  (let* ((rows (org-roam-db-query `[:select [files:file titles:title tags:tags files:meta] :from titles
                                    :left :join files :using [[ file ]]
                                    :left :join tags :using  [[ file ]]
                                    ,@(if conditions
                                          `(:where ,conditions))]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--add-tag-string title tags))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))))

(defun org-roam-search--join-title (title-lt)
  "Join strings within TITLE-LT to one string."
  (if title-lt
      (cl-reduce (lambda (acc x) (concat acc " " x)) title-lt)))

(defun org-roam-search--insert-into-list (lt el n)
  "Insert into list LT an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((n (min (length lt) n))
         (padded-list (cons nil lt))
         (c (nthcdr n padded-list)))
    (setcdr c (cons el (cdr c)))
    (cdr padded-list)))

(defun org-roam-search-completion--helm-candidate-transformer (candidates _source)
  "Transforms CANDIDATES for Helm-based completing read.
SOURCE is not used."
  (let ((prefix (propertize "[?] "
                            'face 'helm-ff-prefix)))
    (org-roam-search--insert-into-list
     candidates
     (propertize helm-pattern
                 'display (concat prefix (condition-case nil (--> helm-pattern
                                                                  (org-roam-search--query-string-to-sexp it)
                                                                  (org-roam-search--stringify-query it)
                                                                  (org-roam--add-tag-string
                                                                   (org-roam-search--join-title
                                                                    (plist-get it :title))
                                                                   (plist-get it :tags)))
                                           (error ""))))
     org-roam-search-prefix-index)))

;;;; org-roam to delve and back export ::
(defun delve-export-org-roam ()
  "Export delve items into a org roam popup buffer."
  (interactive)
  (org-roam-search-find-file :initial-prompt org-roam-search--delve-local-query))

(defun helm-export-org-roam ()
  "Export helm org roam buffer into a delve."
  (interactive)
  "show `helm-org-roam' search in an `delve' buffer."
  (let ((query (org-roam-search--query-string-to-sexp helm-pattern)))
    (with-helm-alive-p
      (helm-run-after-exit (lambda () (--> (delve-show--delve-get-page query :include-titles 't :sexp 't :tag-fuzzy 't :title-fuzzy 't)
                                           (delve-new-collection-buffer (-mapcat #'delve-operate-search (delve-create-searches it))
                                                                        (delve--pretty-main-buffer-header)
                                                                        "Roam Export")
                                           (with-current-buffer it
                                             (local-set-key (kbd "M-a E") #'delve-export-org-roam)
                                             (setq-local org-roam-search--delve-local-query helm-pattern)
                                             it)
                                           (switch-to-buffer it))
                             nil)))))

;;;; org roam helm interface ::
(defconst org-roam-search-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-a") #'nil)
    (define-key map (kbd "M-a E") #'helm-export-org-roam) ;; embark type export
    map)
  "Keymap for `org-roam-search'.")

;;;; helm search completion ::
(defun org-roam-search--get-file (candidate)
  "Get file path associated with CANDIDATE."
  (plist-get candidate :path))

(cl-defun org-roam-search-completion--completing-read (prompt choices &key
                                                              require-match initial-input
                                                              action)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT.
If REQUIRE-MATCH is t, the user must select one of the CHOICES.
Return user choice."
  (let ((source (helm-make-source prompt 'helm-source-sync
                  :candidates (lambda ()
                                (condition-case nil
                                    (--> helm-pattern
                                         (org-roam-search--query-string-to-sexp it)
                                         (org-roam-search--transform-query it)
                                         (org-roam-search--get-title-path-completions it))
                                  (error
                                   choices)))
                  :match #'identity
                  :fuzzy-match nil
                  :multimatch nil
                  :nohighlight t
                  :keymap 'org-roam-search-map
                  :history 'org-roam-search-history
                  :action `(("default" . ,(if action
                                              action
                                            #'(lambda (_)
                                                (helm-marked-candidates)))))
                  :persistent-action  #'(lambda (candidate) (-->
                                                             (org-roam-search--get-file candidate)
                                                             (if (find-buffer-visiting it)
                                                                 (org-roam--find-file it)
                                                               (--> (org-roam--find-file it)
                                                                    (add-to-list 'org-roam-search--kill-buffers-list it)))))
                  :volatile t
                  :group 'org-roam
                  :filtered-candidate-transformer
                  (and (not require-match)
                       #'org-roam-search-completion--helm-candidate-transformer)))
        (buf (concat "*org-roam-search "
                     (s-downcase (s-chop-suffix ":" (s-trim prompt)))
                     "*")))
    (or (let ((res (helm :sources source
                         :input initial-input
                         :prompt prompt
                         :buffer buf)))
          (dolist (buf org-roam-search--kill-buffers-list)
            (kill-buffer buf))
          (setq org-roam-search--kill-buffers-list 'nil)
          res)
        (keyboard-quit))))

(defmacro org-roam-search--common-create-file (type)
  "Macro to Create file based on TYPE."
  (declare (indent 2) (debug t))
  `(when-let* ((title-tags-plist (-some->> result
                                   (org-roam-search--query-string-to-sexp)
                                   (org-roam-search--stringify-query)))
               (title (org-roam-search--join-title (plist-get title-tags-plist :title)))
               (aliases (combine-and-quote-strings (plist-get title-tags-plist :aliases)))
               (tags (--> (plist-get title-tags-plist :tags)
                          (append org-roam-search-default-tags it)
                          (seq-uniq it)
                          (combine-and-quote-strings it)))
               (org-roam-capture--info `((title . ,title)
                                         (aliases . ,aliases)
                                         (tags . ,tags)
                                         (slug  . ,(funcall org-roam-meta-to-slug-function `(:titles ,(cons title
                                                                                                            (plist-get title-tags-plist :aliases))
                                                                                             :tags ,(plist-get title-tags-plist :tags))))))
               (org-roam-capture-additional-template-props (append additional-props
                                                                   ,(pcase type
                                                                      (''find   '(list :finalize 'find-file))
                                                                      (''insert '(list :region (org-roam-shield-region beg end)
                                                                                       :insert-at (point-marker)
                                                                                       :link-type link-type
                                                                                       :link-description title
                                                                                       :finalize 'insert-link)))))
               (org-roam-capture--context 'title)
               (org-roam-capture-templates (or template '(("l" "from link" plain (function org-roam--capture-get-point)
                                                           "%?"
                                                           :file-name "${slug}"
                                                           :head "#+title: ${title}
#+roam_alias: ${aliases}
#+roam_tags: ${tags}
#+roam_keys:
#+created: %U
#+last_modified: %U \n"
                                                           :immediate-finish t
                                                           :unnarrowed t)))))
     (org-roam-capture--capture arg)))

(cl-defun org-roam-search-find-file (arg &key initial-prompt completions filter-fn no-confirm template additional-props)
  "Find and open an Org-roam file passing ARG to `org-roam-capture'.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt.
TEMPLATE is the custom template to use for `org-roam-capture'.
ADDITIONAL-PROPS modifies default template."
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam-search--get-title-path-completions))))
         (results (if no-confirm
                      initial-prompt
                    (org-roam-search-completion--completing-read "File: " completions
                                                                 :initial-input initial-prompt))))
    (dolist (result results)
      (let ((file-path (plist-get result :path)))
        (if file-path
            (org-roam--find-file file-path)
          (org-roam-search--common-create-file 'find))))))

;;;###autoload
(cl-defun org-roam-search-insert (arg &key lowercase completions filter-fn description link-type template additional-props)
  "Find an Org-roam file and insert org link passing ARG to `org-roam-capture'.
Insert a relative org link to it at point and Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
LINK-TYPE is the type of link to be created. It defaults to \"file\".
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label.  See
`org-roam--get-title-path-completions' for details.
TEMPLATE is the custom template to use for `org-roam-capture'.
ADDITIONAL-PROPS modifies default template."

  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (completions (--> (or completions
                                     (org-roam--get-title-path-completions))
                                 (if filter-fn
                                     (funcall filter-fn it)
                                   it)))
               (results (org-roam-search-completion--completing-read "File: " completions
                                                                     :initial-input region-text)))
          (dolist (result results return-file-path)
            (let* (region-text
                   beg end
                   (_ (when (region-active-p)
                        (setq beg (set-marker (make-marker) (region-beginning)))
                        (setq end (set-marker (make-marker) (region-end)))
                        (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
                   (target-file-path (plist-get result :path)))
              (cond ((and target-file-path
                          (file-exists-p target-file-path))
                     (when region-text
                       (delete-region beg end)
                       (set-marker beg nil)
                       (set-marker end nil))
                     (unless (equal result (car results))
                       (insert " "))
                     (let* ((description (or description region-text (plist-get result :title)))
                            (description (if lowercase
                                             (downcase description)
                                           description)))
                       (insert (org-roam-format-link target-file-path description link-type))
                       (setq return-file-path target-file-path)))
                    (t
                     (org-roam-search--common-create-file 'insert)
                     (setq return-file-path (org-roam-capture--get :file-path))))))))
    (deactivate-mark)))

(provide 'org-roam-search)
;;; org-roam-search.el ends here
