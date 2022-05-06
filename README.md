# org-roam-search
org-ql like search for org-roam.
depends on [sexp-string](https://github.com/natask/sexp-string)

# Motivation
I wanted a better search engine for my org roam files. I was tired of fiddling with parenthesises to match tags only when using org-roam-find-file and didn't like that I couldn't make a boolean query. 

# Usage
run `org-roam-search-node-find` or `org-roam-search-node-insert`. They are drop in replacements for `org-roam-node-find` and `org-roam-node-insert`.

type in `tag:tag1 search string` is a valid search that displays nodes with tag `tag1` and contain `search string`.

# Features
- ability to search for file by tag and by title distinctively.
- boolean matching, combining terms with either \`and and \`or. Defaults to combining terms with and.

for example,
``tag:query1,query2 `or title:query3 `and query4``

searches for files that have tag query1 and query2 or have an alias/title matching query3, and also have either query4 as a title or a tag.

- integrate with [delve-show](https://github.com/natask/delve-show) to export org roam buffers into persistent form.
 
## supported key words
- tag
- title
- olp (path to node)
- destination (node that is pointed by link)
- source (node that points to link) 
