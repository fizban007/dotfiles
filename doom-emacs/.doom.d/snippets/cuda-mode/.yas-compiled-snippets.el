;;; Compiled snippets and support files for `cuda-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cuda-mode
                     '(("ifndef" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}\n#define $1\n\n$0\n\n#endif  // $1" "preprocessor-ifndef" nil nil nil "/home/alex/.doom.d/snippets/cuda-mode/prep-ifndef" nil nil)
                       ("cfl" "// ----- $1 ----- $0" "line comment" nil nil nil "/home/alex/.doom.d/snippets/cuda-mode/comment-line" nil nil)
                       ("cff" "////////////////////////////////////////////////////////////////////////////////\n///  $1\n///  \\param $2\n///  \\return $0\n////////////////////////////////////////////////////////////////////////////////" "comment-func" nil nil nil "/home/alex/.doom.d/snippets/cuda-mode/comment-function" nil nil)
                       ("cfr" "////////////////////////////////////////////////////////////////////////////////\n///  $0\n////////////////////////////////////////////////////////////////////////////////" "comment-frame" nil nil nil "/home/alex/.doom.d/snippets/cuda-mode/comment-frame" nil nil)
                       ("cfd" "/////////////////////////////////////////////////////////////////////////////////////////\n///\n///           \\file  `(file-name-nondirectory (buffer-file-name))`\n///\n/// __Description__:     $0\n///\n/// __Last modified__:   <>\\n\n/// __Version__:         1.0\\n\n/// __Author__:          `(user-full-name)`, `(print user-mail-address)`\\n\n/// __Organization__:    `(print user-organization)`\n///\n/////////////////////////////////////////////////////////////////////////////////////////\n" "comment-desc" nil nil nil "/home/alex/.doom.d/snippets/cuda-mode/comment-desc" nil nil)
                       ("class" "class ${1:Name}\n{\npublic:\n    ${1:$(yas-substr yas-text \"[^: ]*\")}($2);\n    virtual ~${1:$(yas-substr yas-text \"[^: ]*\")}();\n}; // ----- end of class $1 -----\n$0" "class customized" nil nil nil "/home/alex/.doom.d/snippets/cuda-mode/class" nil nil)))


;;; Do not edit! File generated at Sun Apr 28 10:20:24 2019
