;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("title" "#+TITLE: ${1:title}\n" "Title Block" nil nil nil "/home/alex/.doom.d/snippets/org-mode/title" nil nil)
                       ("text" "#+TEXT: ${1:text}\n" "Text" nil nil nil "/home/alex/.doom.d/snippets/org-mode/text" nil nil)
                       ("tags" "#+TAGS: $0\n" "Tags" nil nil nil "/home/alex/.doom.d/snippets/org-mode/tags" nil nil)
                       ("startup" "#+STARTUP: ${1:options}\n" "Startup" nil nil nil "/home/alex/.doom.d/snippets/org-mode/startup" nil nil)
                       ("src" "#+BEGIN_SRC $1$>\n$0$>\n#+END_SRC$>\n" "Source Code Block" nil nil nil "/home/alex/.doom.d/snippets/org-mode/src" nil nil)
                       ("seq" "#+SEQ_TODO: ${1:STATES} | ${2:FINISHED}\n" "SEQ TODO" nil nil nil "/home/alex/.doom.d/snippets/org-mode/seq" nil nil)
                       ("sb" "#+srcname: ${1:name}\n#+begin_src ${2:language} $3\n  $0\n#+end_src\n" "#+srcname:..#+begin_src...#+end_src" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/home/alex/.doom.d/snippets/org-mode/sb" nil nil)
                       ("res" "#+RESNAME:\n\n" "Org-Babel RESNAME Block" nil nil nil "/home/alex/.doom.d/snippets/org-mode/res" nil nil)
                       ("prop" " :PROPERTIES:\n :VISIBILITY:folded:\n :END:\n" "Properties Folded" nil nil nil "/home/alex/.doom.d/snippets/org-mode/prop" nil nil)
                       ("options" "#+OPTIONS: ${0}\n\n" "OPTIONS" nil nil nil "/home/alex/.doom.d/snippets/org-mode/options" nil nil)
                       ("latex" "#+BEGIN_LATEX\n${0}\n#+END_LATEX\n" "LATEX" nil nil nil "/home/alex/.doom.d/snippets/org-mode/latex" nil nil)
                       ("lang" "#+LANGUAGE: ${1:en}\n" "LANGUAGE" nil nil nil "/home/alex/.doom.d/snippets/org-mode/lang" nil nil)
                       ("keywords" "#+KEYWORDS: ${0}\n" "KEYWORDS" nil nil nil "/home/alex/.doom.d/snippets/org-mode/keywords" nil nil)
                       ("inc" "#+INCLUDE: \"${1:file}\" ${2:src-example-quote} ${3:mode}\n\n" "Author" nil nil nil "/home/alex/.doom.d/snippets/org-mode/inc" nil nil)
                       ("html" "#+BEGIN_HTML\n${0}\n#+END_HTML\n" "HTML" nil nil nil "/home/alex/.doom.d/snippets/org-mode/html" nil nil)
                       ("email" "#+EMAIL: ${1:`user-mail-address`}\n" "Email" nil nil nil "/home/alex/.doom.d/snippets/org-mode/email" nil nil)
                       ("orgemacs" "*** Elisp code\n#+BEGIN_SRC emacs-lisp :tangle ~/.emacs.d/init.d/${1:filename}\n$0\n#+END_SRC\n*** Include into main file\n#+BEGIN_SRC emacs-lisp\n(load-file \"~/.emacs.d/init.d/$1\")\n#+END_SRC" "Orgemacs-entry" nil nil nil "/home/alex/.doom.d/snippets/org-mode/emacs-entry" "direct-keybinding" nil)
                       ("docbook" "#+BEGIN_DOCBOOK\n${0}\n#+END_DOCBOOK\n" "DOCBOOK" nil nil nil "/home/alex/.doom.d/snippets/org-mode/docbook" nil nil)
                       ("dita" "#+BEGIN_DITAA ${1:export-file-name} -r -S -E\n${0}\n#+END_DITAA\n" "DITAA" nil nil nil "/home/alex/.doom.d/snippets/org-mode/dita" nil nil)
                       ("desc" "#+DESCRIPTION: ${0}\n" "DESCRIPTION" nil nil nil "/home/alex/.doom.d/snippets/org-mode/desc" nil nil)
                       ("block" "#+begin_$1 $2\n  $0\n#+end_$1\n" "#+begin_...#+end_" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/home/alex/.doom.d/snippets/org-mode/block" nil nil)
                       ("author" "#+AUTHOR: ${1:`user-full-name`}\n" "Author" nil nil nil "/home/alex/.doom.d/snippets/org-mode/author" nil nil)))


;;; Do not edit! File generated at Sun Apr 28 10:20:24 2019
