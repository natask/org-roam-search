# org-roam-search
org-ql like search for org-roam.
depends on the following packages not yet on melpa. [sexp-string](https://github.com/natask/sexp-string), [delve](https://github.com/publicimageltd/delve), [delve-show](https://github.com/natask/delve-show).

# Motivation
I wanted a better search engine for my org roam files. I was tired of fiddling with parenthesises to match tags only when using org-roam-find-file and didn't like that I couldn't make a boolean query. 

# Installation
The package is not on melpa yet. In the mean time you can try it out quickly with the following instructions and install it adding to `load-path` or manually by executing `load-directory` on a directory containing all the relevent files or through utilizing your emacs distributions prefered way of installing packages from github.  

## Quick Exprementation
copy paste into a buffer and run `eval-buffer` on  [sexp-string code](https://github.com/natask/sexp-string/blob/master/sexp-string.el) and  [org-roam-search code](https://github.com/natask/org-roam-search/blob/master/org-roam-search.el) after removing the line `(require delve-show)` from `org-roam-search code`. `delve-show` is not critical for the operation of this package.

## Installation on doom emacs
look through [doom-emacs installation guide](https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#package-management)
Add the following to `~/.doom.d/package.el`.
``` elisp
(package! org-roam-search
  :recipe (:host github
          :repo "natask/org-roam-search"
          :branch "master"))

(package! sexp-string
  :recipe (:host github
          :repo "natask/sexp-string"
          :branch "master"))

(package!  delve
  :recipe (:host github
           :repo "publicimageltd/delve"
           :branch "main"
           :files ("*.el")))
(package!  delve-show
  :recipe (:host github
           :repo "natask/delve-show"
           :branch "master"
           :files ("*.el")))
```
and place the following in `~/.doom.d/config.el`
``` elisp
(use-package org-roam-search
    :after (org-roam)
    ;:custom
    ;(org-roam-search-default-tags '("stub"))
    :bind (:map global-map
          (("C-c n f" . org-roam-search-node-find))
            :map org-mode-map
          (("C-c n i" . org-roam-search-node-insert)))
    )
```
# Usage
run `org-roam-search-node-find` or `org-roam-search-node-insert`. They are drop in replacements for `org-roam-node-find` and `org-roam-node-insert`.

type in `tag:tag1 search string`. It is a valid search that displays nodes with tag `tag1` and contain `search string`.

# Features
- ability to search for file by tag and by title distinctively.
- boolean matching, combining terms with either \`and and \`or. Defaults to combining terms with and.

for example,
``tag:query1,query2 `or title:query3 `and query4``

searches for files that have tag query1 and query2 or have an alias/title matching query3, and also have either query4 as a title or a tag.

- integrate with [delve-show](https://github.com/natask/delve-show) to export org roam buffers into persistent form.
 
## Supported key words
- tag
- title
- olp (path to node)
- context (olp plus tag)
- all (title and context)
- level (node level)
- destination (node that is pointed by link)
- source (node that points to link) 

look at https://github.com/natask/sexp-string#custom-pexs for syntax.
