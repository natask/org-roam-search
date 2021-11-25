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
              `(or
                ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like title ,(rec elem)))) (cons 'and rest))
                ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like aliases '(,(rec elem))))) (cons 'and rest)))))
            :stringify
            ((`(,(or 'titles 'title 'aliases 'alias) . ,rest)
              (plist-put accum :aliases (append (plist-get accum :aliases) rest)))))
    (tags :name tags :aliases '(tag context)
          :transform
          ((`(,(or 'tags 'tag) . ,rest)
            `(or
              ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like tags '(,(rec elem))))) (cons 'and rest))
              ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like olp '(,(rec elem))))) (cons 'and rest)))))
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
           (((pred stringp) (concat "%%" element "%%")))
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
(defvar org-roam-search-node-display-template (concat (propertize "${tags}" 'face 'org-tag) " ${title}")
  "See `org-roam-node-display-template'.")
(defvar org-roam-search-max 50
  "Max number of items displayed as candidates.")
(defvar org-roam-search-default-tags 'nil
  "List of tags to add to each file created.")
(defvar org-roam-search-default-templates  '(("l" "from link" plain "%?"
                                              :target  (file+head "${slug}.org"
                                                                  ":PROPERTIES:
:roam_refs: ${refs}
:roam_aliases: ${aliases}
:END:
#+title: ${title}
#+filetags: ${tags}
#+roam_keys:
#+created: %U
#+last_modified: %U \n")
                                        ;:immediate-finish t
                                              :unnarrowed t
                                              ))
  "See `org-roam-capture-templates'.")

;;; Code:
(declare-function org-roam-search--query-string-to-sexp "ext:org-roam-search" (query) t)
(declare-function org-roam-search--transform-query "ext:org-roam-search" (query) t)
(declare-function org-roam-search--stringify-query "ext:org-roam-search" (query) t)
(fset 'org-roam-search--query-string-to-sexp
      (sexp-string--define-query-string-to-sexp-fn  "org-roam-search"))
(fset 'org-roam-search--transform-query (sexp-string--define-transform-query-fn "org-roam-search" :transform))
(fset 'org-roam-search--stringify-query (sexp-string--define-transform-query-fn "org-roam-search" :stringify))


;;;; org-roam to delve and back export :: TODO: do manage the delve export
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

(cl-defun org-roam-search-node-read (prompt choices &key
                                            filter-clause
                                            sort-clause
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
                                         (org-roam-search-node-list :conditions it :filter-clause filter-clause :sort-clause sort-clause))
                                  (error
                                   choices)))
                  :match #'identity
                  :candidate-transformer (lambda (nodes)
                                           (mapcar
                                            (lambda (node) (--> (org-roam-node-read--to-candidate node (org-roam-node--process-display-format org-roam-node-display-template))
                                                                (-let* (((title . node) it)
                                                                        (title (concat (substring-no-properties title)
                                                                                       " "
                                                                                       (funcall org-roam-node-annotation-function node))))  ;;NOTE: add to title. can also get node from (get-text-property 0 'node title)
                                                                  (cons title node)))) nodes))
                  :fuzzy-match nil
                  :multimatch nil
                  :nohighlight t
                  :keymap 'org-roam-search-map
                  :history 'org-roam-search-history
                  :action `(("default" . ,(if action
                                              action
                                            #'(lambda (_)
                                                (helm-marked-candidates)))))
                  :persistent-action  #'(lambda (candidate) (let ((condition (find-buffer-visiting (org-roam-node-file candidate)))
                                                                  (buffer (org-roam-node-visit candidate 't)))
                                                              (unless condition
                                                                (add-to-list 'org-roam-search--kill-buffers-list buffer))))
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
          (car res))
        (keyboard-quit))))

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
  (let* ((node-props  (condition-case nil (--> helm-pattern
                                               (org-roam-search--query-string-to-sexp it)
                                               (org-roam-search--stringify-query it)
                                               (plist-put it :title (org-roam-search--join-title (plist-get it :title))))
                        (error '(:title "" :tags nil))))
         (prefix (propertize "[?]"             'face 'helm-ff-prefix))
         (element (org-roam-node-read--to-candidate
                   (apply 'org-roam-node-create node-props) (org-roam-node--process-display-format org-roam-search-node-display-template)))
         (element (cons (concat prefix (car element)) (cdr element))))
    (org-roam-search--insert-into-list
     candidates
     element
     org-roam-search-prefix-index)))

(cl-defun org-roam-search-node-list (&key conditions filter-clause sort-clause limit)
  "Return LIMIT or `org-roam-search-max' nodes stored in the database matching CONDITIONS and FILTER-CLAUSE sorted by SORT-CLAUSE as a list of `org-roam-node's."
  (let* ((conditions-clause (if conditions
                                (car (emacsql-prepare `[,conditions]))))
         (constraint-clause (pcase (cons conditions-clause filter-clause)
                        (`(nil . ,_) filter-clause)
                        (`(,_ . nil) conditions-clause)
                        (`(,_ . ,_) (string-join (list "(" conditions-clause ")" "AND" "(" filter-clause ")") " "))))
         (where-clause (if constraint-clause
                           (concat "WHERE " constraint-clause)))
         (order-by-clause (pcase sort-clause  ;;[(desc love) (asc this)]]
                            ((pred vectorp)
                             (car (emacsql-prepare `[:order-by ,sort-clause])))
                            ((pred stringp)
                            (concat "ORDER BY " sort-clause))
                            (_ sort-clause)))
         (limit-clause (if (or limit org-roam-search-max)
                           (format "limit %d" (or limit org-roam-search-max))))
         (query (string-join
                 (list
                  "SELECT id, file, filetitle, level, todo, pos, priority,
           scheduled, deadline, title, properties, olp, atime,
           mtime, tags, aliases, refs FROM
           -- from clause
             (
             SELECT  nodes.id as id,  nodes.file as file,  nodes.level as level,
               nodes.todo as todo,   nodes.pos as pos,  nodes.priority as priority,
               nodes.scheduled as scheduled,  nodes.deadline as deadline,  nodes.title as title,
               nodes.properties as properties,  nodes.olp as olp,  files.atime as atime,
               files.title as filetitle,
               files.mtime as mtime,  '(' || group_concat(tags.tag, ' ') || ')' as tags, '(' || group_concat(aliases.alias, ' ') || ')' as aliases,
               '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
             FROM nodes
             LEFT JOIN files ON files.file = nodes.file
             LEFT JOIN tags ON tags.node_id = nodes.id
             LEFT JOIN aliases ON aliases.node_id = nodes.id
             LEFT JOIN refs ON refs.node_id = nodes.id
             GROUP BY nodes.id)
             -- end from clause"
                  where-clause
                  order-by-clause
                  limit-clause) "\n"))
         (rows (org-roam-db-query query)))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-title file-title
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :aliases aliases
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

;;;###autoload
(cl-defun org-roam-search-node-find (&optional other-window initial-input filter-clause &key sort-clause templates)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-CLAUSE is a filter string that is compatible with sql query.
SORT-CLAUSE is a sort string that is compatible with sql query.
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)"
  (interactive current-prefix-arg)
  (let ((node (org-roam-search-node-read "Search node:" (org-roam-search-node-list :filter-clause filter-clause :sort-clause sort-clause) :initial-input initial-input :filter-clause filter-clause :sort-clause sort-clause))
        (templates (or templates org-roam-search-default-templates)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :templates templates
       :props '(:finalize find-file)))))

;;;###autoload
(cl-defun org-roam-search-node-insert (&optional filter-clause &key sort-clause templates info)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-CLAUSE is a filter string that is compatible with sql query.
SORT-CLAUSE is a sort string that is compatible with sql query.
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-search-node-read "Insert node:" (org-roam-search-node-list :filter-clause filter-clause :sort-clause sort-clause) :initial-input region-text :filter-clause filter-clause :sort-clause sort-clause))
               (description (or region-text
                                (org-roam-node-formatted node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (org-roam-capture-
             :node node
             :info info
             :templates (or templates org-roam-search-default-templates)
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))

(provide 'org-roam-search)
;;; org-roam-search.el ends here
