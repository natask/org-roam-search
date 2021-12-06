;;; org-roam-search.el --- Search through org roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: August 30, 2021
;; Modified: August 30, 2021
;; Version: 0.0.2
;; Keywords: matching,org-roam
;; Homepage: https://github.com/savnkk/org-roam-search
;; Package-Requires: ((emacs "26.1") (org "9.3") (org-roam "2") (sexp-string "0.0.1") (delve-show) (helm))
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
(defvar org-roam-search-predicates 'nil
  "Predicate list to convert string to sexp.")
(defvar org-roam-search-default-boolean 'and
  "Predicate default binary function.")
(defvar org-roam-search-default-predicate 'all
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
                                              :immediate-finish t
                                              :unnarrowed t))
  "See `org-roam-capture-templates'.")

;;; Code:
(defun org-roam-search--boolean-transform (boolean)
  "Transform expression for BOOLEAN predicate."
  `((`(,',boolean . ,clauses) (if-let ((clauses (-non-nil (mapcar #'rec clauses))))
                                  `(,',boolean ,@clauses)))))

(defun org-roam-search--define-predicates ()
  "Define `org-roam-search-predicates'."
  (setq org-roam-search-predicates
        `((or  :name or
               :transform
               ,(org-roam-search--boolean-transform 'or)
               :transform-source
               ,(org-roam-search--boolean-transform 'or)
               :transform-destination
               ,(org-roam-search--boolean-transform 'or)
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
          (not :name not
               :transform
               ,(org-roam-search--boolean-transform 'not)
               :transform-source
               ,(org-roam-search--boolean-transform 'not)
               :transform-destination
               ,(org-roam-search--boolean-transform 'not))
          (and :name and
               :transform
               ,(org-roam-search--boolean-transform 'and)
               :transform-source
               ,(org-roam-search--boolean-transform 'and)
               :transform-destination
               ,(org-roam-search--boolean-transform 'and)
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
                      ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like aliases ',(rec elem)))) (cons 'and rest)))))
                  :stringify
                  ((`(,(or 'titles 'title 'aliases 'alias) . ,rest)
                    (plist-put accum :aliases (append (plist-get accum :aliases) rest)))))
          (tags :name tags :aliases '(tag)
                :transform
                ((`(,(or 'tags 'tag) . ,rest)
                  (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like tags ',(rec elem)))) (cons 'and rest))))
                :stringify
                ((`(,(or 'tags 'tag) . ,rest)
                  (plist-put accum :tags (-concat (plist-get accum :tags) rest)))))
          (olp :name olp
               :transform
               ((`(olp . ,rest)
                 `(or
                   ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like filetitle ',(rec elem)))) (cons 'and rest))
                   ,(-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like olp ',(rec elem)))) (cons 'and rest)))))
               :stringify
               ((`(olp . ,rest)
                 (plist-put accum :tags (-concat (plist-get accum :tags) rest)))))
          (context :name context
                   :transform
                   ((`(context . ,rest)
                     (rec `(or (tags ,@rest)
                               (olp ,@rest)))))
                   :stringify
                   ((`(context . ,rest)
                     (plist-put accum :tags (-concat (plist-get accum :tags) rest)))))
          (destination :name destination :aliases '(dest)
                       :transform-destination
                       ((`(,(or 'dest 'destination) . ,rest)
                         (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like dest ,(rec (s-replace-regexp org-link-plain-re "\\2" elem))))) (cons 'and rest)))))
          (source :name source
                  :transform-source
                  ((`(source . ,rest)
                    (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(like source ,(rec (s-replace-regexp org-link-plain-re "\\2" elem))))) (cons 'and rest)))))
          (level :name level :aliases '(l d depth)
                 :transform
                 ((`(,(or 'level 'l 'd 'depth) . ,rest)
                   (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(= level ,(string-to-number elem)))) (cons 'and rest)))))
          (all :name all
               :transform
               ((`(all . ,rest)
                 (rec `(or (context ,@rest)
                           (titles ,@rest)))))
               :search
               ((`(all . ,rest)
                 (list :title (plist-get (rec `(titles ,@rest) accum) :title)
                       :tags (plist-get (rec `(context ,@rest) accum) :tags))))
               :stringify
               ((`(all . ,rest)
                 (plist-put accum :title (-concat (plist-get accum :title) rest)))))
          (skip :transform
                (((pred listp)))
                :transform-source
                (((pred listp)))
                :transform-destination
                (((pred listp))))
          (query :name query
                 :transform
                 (((pred stringp) (concat "%%" element "%%")))
                 :transform-source
                 (((pred stringp) (concat "%%" element "%%")))
                 :transform-destination
                 (((pred stringp) (concat "%%" element "%%")))
                 :search
                 (((pred stringp) element))
                 :stringify
                 (((pred stringp) element))))))

(declare-function org-roam-search--query-string-to-sexp "ext:org-roam-search" (query) t)
(declare-function org-roam-search--transform-query "ext:org-roam-search" (query) t)
(declare-function org-roam-search--stringify-query "ext:org-roam-search" (query) t)
(org-roam-search--define-predicates)
(fset 'org-roam-search--query-string-to-sexp (sexp-string--define-query-string-to-sexp-fn "org-roam-search"))
(fset 'org-roam-search--transform-query (sexp-string--define-transform-query-fn "org-roam-search" :transform))
(fset 'org-roam-search--transform-source-query (sexp-string--define-transform-query-fn "org-roam-search" :transform-source))
(fset 'org-roam-search--transform-destination-query (sexp-string--define-transform-query-fn "org-roam-search" :transform-destination))
(fset 'org-roam-search--stringify-query (sexp-string--define-transform-query-fn "org-roam-search" :stringify))

(defun org-roam-search-map-entry (type)
  `(= level ,(string-to-number elem)
      (-tree-map (lambda (elem) (if (member elem '(or and)) elem type))) (cons 'and rest)))

;;;; org-roam to delve and back export
(defun org-roam-search-import-from-delve ()
  "Import from delve items into a org roam popup buffer."
  (interactive)
  (org-roam-search-node-find :initial-input org-roam-search--delve-local-query))

(defun org-roam-search-export-to-delve ()
  "Export helm org roam buffer into a delve buffer."
  (interactive)
  "show `helm-org-roam' search in an `delve' buffer."
  (let ((query helm-pattern))
    (with-helm-alive-p
      (helm-run-after-exit (lambda () (--> (delve-show-create-query query :include-titles 't :tag-fuzzy 't :title-fuzzy 't)
                                           (switch-to-buffer (delve--new-buffer "Roam Export" (list it)))
                                           (with-current-buffer it
                                             (local-set-key (kbd "M-a E") #'org-roam-search-import-from-delve)
                                             (setq-local org-roam-search--delve-local-query query)
                                             it)
                                           (switch-to-buffer it))
                             nil)))))

;;;; org roam helm interface ::
(defconst org-roam-search-map
  (let ((map (make-composed-keymap nil minibuffer-local-map)))
    (define-key map (kbd "M-a") #'nil)
    (define-key map (kbd "M-a E") #'org-roam-search-export-to-delve) ;; embark type export
    map)
  "Keymap for `org-roam-search'.")

(setq org-roam-search--current-nodes 'nil)
(setq org-roam-search--current-string "")

(defun org-roam-search-node-read--to-candidate (node template)
  "Return a minibuffer completion candidate given NODE.
TEMPLATE is the processed template used to format the entry."
  (let ((candidate-main (org-roam-node--format-entry
                         template
                         node
                         (1- (frame-width)))))
    (cons (propertize candidate-main 'node node) node)))

(cl-defun org-roam-search-node-read (prompt choices &key
                                            filter-clause
                                            sort-clause
                                            require-match initial-input
                                            action)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT.
If REQUIRE-MATCH is t, the user must select one of the CHOICES.
FILTER-CLAUSE and SORT-CLAUSE are arguments to `org-roam-search-node-list'.
Return user choice."
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((choices (mapcar (lambda (node) (org-roam-search-node-read--to-candidate node (org-roam-node--process-display-format org-roam-node-display-template))) choices))
               (node (completing-read
                      prompt
                      (lambda (string pred action)
                        (cond
                         ((eq (car-safe action) 'boundaries)
                                        ;(print `(boundaries ,(length string) . ,(length (cdr action))))
                          `(boundaries ,(length string) . ,(length (cdr action))))
                         ((eq action 'metadata)
                                        ;(print string)
                          `(metadata
                            .
                            ;; Preserve sorting in the completion UI
                            ((display-sort-function . identity)
                             (cycle-sort-function . identity)
                             (annotation-function
                              . ,(lambda (title)
                                   (funcall org-roam-node-annotation-function
                                            (get-text-property 0 'node title))))
                             (category . org-roam-search-node))))
                         ((null action) ;; try-completion
                          string)
                         ((eq action 'lambda) ;;test-completions
                          't)
                         ((eq action t) ;; all-completions
                          (let ((nodes (cond
                                        ;; ((string-equal (string-trim string) org-roam-search--current-string)
                                        ;;   org-roam-search--current-nodes)
                                        (t
                                         (-as-> (condition-case nil
                                                    (mapcar
                                                     (lambda (node) (org-roam-search-node-read--to-candidate node (org-roam-node--process-display-format org-roam-node-display-template)))
                                                     (org-roam-search-node-list :input-string string
                                                                                :filter-clause filter-clause
                                                                                :sort-clause sort-clause))
                                                  (error
                                                   choices)) nodes
                                                   (if (not require-match)
                                                       (org-roam-search-completion--insert-default-candidate string nodes)
                                                     nodes)
                                                   (prog1
                                                     nodes
                                                     (setq org-roam-search--current-nodes nodes)
                                                     (setq org-roam-search--current-string string)))))))
                            (cl-loop for node in nodes
                                     for y from 1
                                     collect (car node))))))
                      nil require-match initial-input 'org-roam-search-history))
               ;; :keymap 'org-roam-search-map
               ;; :persistent-action  #'(lambda (candidate) (let ((condition (find-buffer-visiting (org-roam-node-file candidate)))
               ;;                                               (buffer (org-roam-node-visit candidate 't)))
               ;;                                         (unless condition
               ;;                                         (add-to-list 'org-roam-search--kill-buffers-list buffer))))
               ;; (dolist (buf org-roam-search--kill-buffers-list) ;
               ;;   (kill-buffer buf))
               ;; (setq org-roam-search--kill-buffers-list 'nil)
               )
          (cdr (assoc node org-roam-search--current-nodes))))))

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

(defun org-roam-search-completion--insert-default-candidate (input-string candidates)
  "Insert default candidate based on INPUT-STRING into CANDIDATES."
  (let* ((node-props  (condition-case nil (--> input-string
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

(defun org-roam-search--join-vecs (&rest vecs)
  "Join emacsql VECS appropriately."
  (cl-reduce
   (lambda (joined-vecs vec)
     (if vec
         (if joined-vecs
             `[,@joined-vecs
               :union
               ,@vec]
           vec)
       joined-vecs))
   vecs
   :initial-value nil))

(cl-defun org-roam-search-node-list (&key (input-string "") filter-clause sort-clause limit)
  "Return LIMIT or `org-roam-search-max' nodes stored in the database matching INPUT-STRING and FILTER-CLAUSE sorted by SORT-CLAUSE as a list of `org-roam-node's.

INPUT-STRING is a string that searchs for nodes according `org-roam-search-predicates'.
SORT-CLAUSE is a sql string that filters nodes.
LIMIT is the maximum resultant nodes."
  (let* ((query-sexp (org-roam-search--query-string-to-sexp input-string))
         (conditions (org-roam-search--transform-query query-sexp))
         (node-source-conditions (org-roam-search--transform-source-query query-sexp))
         (node-destination-conditions (org-roam-search--transform-destination-query query-sexp))
         (conditions-clause (if conditions
                                (car (emacsql-prepare `[,conditions]))))
         (destination-nodes-query (if node-source-conditions
                                      `[:select :distinct [dest]
                                        :from links
                                        :where ,node-source-conditions
                                        :limit ,(or limit org-roam-search-max)]));; TODO: support citations and searching with refs in the future.
         (source-nodes-query (if node-destination-conditions
                                 `[:select :distinct [source]
                                   :from links
                                   :where ,node-destination-conditions
                                   :limit ,(or limit org-roam-search-max)]))
         (nodes-query (org-roam-search--join-vecs source-nodes-query destination-nodes-query))
         (nodes-clause (-some--> nodes-query
                         (org-roam-db-query it)
                         (mapcar (lambda (node)
                                   `(= id ,(car node))) it)
                         (cons 'or it)
                         (emacsql-prepare `[,it])
                         (car it)))
         (constraint-clause (org-roam-search--join-clauses conditions-clause filter-clause nodes-clause))
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
                  "
SELECT id, file, filetitle, level, todo, pos, priority,
  scheduled, deadline, title, properties, olp,
  atime, mtime, tags, aliases, refs FROM
  (
  SELECT id, file, filetitle, \"level\", todo, pos, priority,
    scheduled, deadline, title, properties, olp, atime,
    mtime, '(' || group_concat(tags, ' ') || ')' as tags,
    aliases, refs FROM
    -- outer from clause
      (
      SELECT  id,  file, filetitle, \"level\", todo,  pos, priority,  scheduled, deadline ,
        title, properties, olp, atime,  mtime, tags,
        '(' || group_concat(aliases, ' ') || ')' as aliases,
        refs FROM
        -- inner from clause
          (
          SELECT  nodes.id as id,  nodes.file as file,  nodes.\"level\" as \"level\",
            nodes.todo as todo,   nodes.pos as pos,  nodes.priority as priority,
            nodes.scheduled as scheduled,  nodes.deadline as deadline,  nodes.title as title,
            nodes.properties as properties,  nodes.olp as olp,  files.atime as atime,
            files.title as filetitle,
            files.mtime as mtime,  tags.tag as tags,    aliases.alias as aliases,
            '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
          FROM nodes
            LEFT JOIN files ON files.file = nodes.file
            LEFT JOIN tags ON tags.node_id = nodes.id
            LEFT JOIN aliases ON aliases.node_id = nodes.id
            LEFT JOIN refs ON refs.node_id = nodes.id
          GROUP BY nodes.id, tags.tag, aliases.alias )
        -- end inner from clause
      GROUP BY id, tags )
    -- end outer from clause
  GROUP BY id)"
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

(defun org-roam-search--join-clauses (&rest clauses)
  "Join sql CLAUSES appropriately."
  (cl-reduce
   (lambda (joined-clauses clause)
     (if (and clause (not (string-empty-p clause)))
         (if (and joined-clauses (not (string-empty-p joined-clauses)))
             (string-join
              (list "(" joined-clauses ")"
                    "AND"
                    "(" clause ")")
              " ")
           clause)
       joined-clauses))
   clauses
   :initial-value nil))

;;;###autoload
(cl-defun org-roam-search-node-find (&key other-window initial-input filter-clause sort-clause templates level)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-CLAUSE is a filter string that is compatible with sql query.
SORT-CLAUSE is a sort string that is compatible with sql query.
and when nil is returned the node will be filtered out.
IF LEVEL, limit to nodes with level LEVEL.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)"
  (interactive current-prefix-arg)
  (let* ((level-filter-clause (if level (format "level = %d" level)))
         (filter-clause (org-roam-search--join-clauses filter-clause level-filter-clause))
         (node (org-roam-search-node-read "Search node:" (org-roam-search-node-list :filter-clause filter-clause :sort-clause sort-clause) :initial-input initial-input :filter-clause filter-clause :sort-clause sort-clause))
         (templates (or templates org-roam-search-default-templates)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :templates templates
       :props '(:finalize find-file)))))

;;;###autoload
(defun org-roam-search-file-find (&rest args)
  "Find and open an file level Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-CLAUSE is a filter string that is compatible with sql query.
SORT-CLAUSE is a sort string that is compatible with sql query.
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
taking ARGS."
  (declare (advertised-calling-convention (&key other-window initial-input filter-clause templates) nil))
  (interactive)
  (apply 'org-roam-search-node-find (plist-put args :level 0)))

;;;###autoload
(cl-defun org-roam-search-node-insert (&key filter-clause sort-clause templates info)
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
