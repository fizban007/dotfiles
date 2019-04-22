;;; lang/cpp/config.el --- c, c++, cuda, and obj-c -*- lexical-binding: t; -*-

(defvar +cpp-default-include-paths
  (list "include"
        "includes")
  "A list of default relative paths which will be searched for up from the
current file, to be passed to irony as extra header search paths. Paths can be
absolute. This is ignored if your project has a compilation database.

This is ignored by ccls.")

(defvar +cpp-default-header-file-mode 'c++-mode
  "Fallback major mode for .h files if all other heuristics fail (in
`+cpp-c-c++-objc-mode').")

(defvar +cpp-default-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++14" ; use C++17 draft by default
              (when IS-MAC
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                "-stdlib=libc++")))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.

This is ignored by ccls.")

;;
;; Major modes

(def-package! company-cmake  ; for `cmake-mode'
  :when (featurep! :completion company)
  :after cmake-mode
  :config (set-company-backend! 'cmake-mode 'company-cmake))

;; (def-package! demangle-mode
;;   :hook llvm-mode)

(def-package! company-glsl  ; for `glsl-mode'
  :when (featurep! :completion company)
  :after glsl-mode
  :config (set-company-backend! 'glsl-mode 'company-glsl))

(def-package! modern-cpp-font-lock
  :hook ((c++-mode cuda-mode) . modern-c++-font-lock-mode))

;;
;; Packages

(def-package! google-c-style)

(def-package! highlight-doxygen
  :init
  (highlight-doxygen-global-mode 1))

(add-hook 'c-mode-common-hook
	  (lambda () (progn
		       (google-set-c-style)
		       (setq company-backends (remove 'company-semantic company-backends))
		       (setq company-backends (remove 'company-clang company-backends))
		       (setq company-backends (remove 'company-xcode company-backends))
		       (setq company-backends (remove 'company-eclim company-backends))
		       (set-electric! '(c-mode c++-mode objc-mode java-mode cuda-mode) :chars '(?\n ?\} ?\{))
		       (set-docsets! 'c-mode "C")
		       (set-docsets! 'c++-mode "C++" "Boost")
		       ) ))

(def-package! cc-mode
  ;; :commands (c-mode c++-mode objc-mode java-mode)
  ;; :mode ("\\.mm\\'" . objc-mode)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "doom")

  ;; The plusses in c++-mode can be annoying to search for ivy/helm (which reads
  ;; queries as regexps), so we add these for convenience.
  (defalias 'cpp-mode 'c++-mode)
  (defvaralias 'cpp-mode-map 'c++-mode-map)

  ;; Activate `c-mode', `c++-mode' or `objc-mode' depending on heuristics
  (add-to-list 'auto-mode-alist '("\\.h\\'" . +cpp-c-c++-objc-mode))

  ;; Ensure find-file-at-point works in C modes, must be added before irony
  ;; and/or lsp hooks are run.
  ;; (add-hook! (c-mode-local-vars c++-mode-local-vars objc-mode-local-vars cuda-mode-local-vars)
  ;;   #'+cpp|init-ffap-integration)

  :config
  ;; (set-electric! '(c-mode c++-mode objc-mode java-mode cuda-mode) :chars '(?\n ?\} ?\{))
  ;; (set-docsets! 'c-mode "C")
  ;; (set-docsets! 'c++-mode "C++" "Boost")

  (set-rotate-patterns! 'c++-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))

  (set-pretty-symbols! '(c-mode c++-mode cuda-mode)
    ;; Functional
    ;; :def "void "
    ;; Types
    :null "nullptr"
    :true "true" :false "false"
    :int "int" :float "float"
    :str "std::string"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    :yield "#require")

  ;;; Better fontification (also see `modern-cpp-font-lock')
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! (c-mode c++-mode cuda-mode) #'+cpp|fontify-constants)

  ;; Custom style, based off of linux
  (c-add-style
   "doom" '((c-basic-offset . tab-width)
            (c-comment-only-line-offset . 0)
            (c-hanging-braces-alist (brace-list-open)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             (case-label . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             (arglist-close +cpp-lineup-arglist-close 0)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             (inclass +cpp-c++-lineup-inclass +)
             (label . 0))))

  ;;; Keybindings
  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (define-key! c++-mode-map "<" nil ">" nil)
  ;; ...and leave it to smartparens
  (sp-with-modes '(c++-mode objc-mode cuda-mode)
    (sp-local-pair "<" ">"
                   :when '(+cpp-sp-point-is-template-p +cpp-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))

  (sp-with-modes '(c-mode c++-mode objc-mode java-mode cuda-mode)
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

;;
;; LSP
(use-package lsp-mode :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(cuda-mode . "cpp"))
  )
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(def-package! ccls
  :when (featurep! +lsp)
  ;; :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars cuda-mode-local-vars) . +cpp|init-ccls)
  :hook ((c-mode c++-mode objc-mode cuda-mode) . +cpp|init-ccls)
  :init
  (defun +cpp|init-ccls ()
    ;; (setq-local company-transformers nil)
    (setq-local company-lsp-async t)
    (setq-local company-lsp-cache-candidates nil)
    (require 'ccls)
    (lsp)
    (map! "M-?" #'lsp-find-references
          "M-." #'lsp-find-definition)
    (setq ccls-args '("--log-file=/tmp/ccls.log" "-v=1" "--init={\"cacheFormat\": \"json\"}"))
    ;; (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
    (setq ccls-sem-highlight-method 'font-lock)
    )
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root"))
    ;; (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
  )
