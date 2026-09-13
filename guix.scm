;;; guix.scm --- Guix package for org-x

(define-module (org-x)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix build-system emacs)
  #:use-module (guix search-paths)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages tree-sitter)
  #:use-module (packages generate)
  #:use-module ((guix build emacs-build-system)
                #:select (%default-include %default-exclude))
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define %emacs-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build emacs-build-system)
    (guix build emacs-utils)
    ,@%default-gnu-imported-modules))

;; function to get a version of emacs for the build
(define (default-emacs)
  "Return the default Emacs package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((emacs-mod (resolve-interface '(gnu packages emacs))))
    (module-ref emacs-mod 'emacs)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (emacs (default-emacs))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:emacs #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("emacs" ,emacs)
                         ,@native-inputs))
         (outputs outputs)
         (build emacs-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (emacs-build store name inputs
                      #:key source
                      (tests? #f)
                      (parallel-tests? #t)
                      (test-command ''("make" "check"))
                      (phases '(@ (guix build emacs-build-system)
                                  (modify-phases %standard-phases)))
                      (outputs '("out"))
                      (include (quote %default-include))
                      (exclude (quote %default-exclude))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %emacs-build-system-modules)
                      (modules '((guix build emacs-build-system)
                                 (guix build utils)
                                 (guix build emacs-utils))))
  "Build SOURCE using EMACS, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (emacs-build #:name ,name
                    #:source ,(match (assoc-ref inputs "source")
                                (((? derivation? source))
                                 (derivation->output-path source))
                                ((source)
                                 source)
                                (source
                                 source))
                    #:system ,system
                    #:test-command ,test-command
                    #:tests? ,tests?
                    #:phases ,phases
                    #:outputs %outputs
                    #:include ,include
                    #:exclude ,exclude
                    #:search-paths ',(map search-path-specification->sexp
                                          search-paths)
                    #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (gexp->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define emacs-build-system
  (build-system
    (name 'emacs)
    (description "The build system for Emacs packages")
    (lower lower)))

(define-public org-x
  (package
   (name "org-x")
   (version "0.0.0")
   (source (local-file (getcwd) #:recursive? #t))
   (build-system emacs-build-system)
   (inputs (list
	    emacs-dash
	    emacs-s
	    emacs-compat
	    generate
	    tree-sitter-typescript
	    tree-sitter-javascript
	    tree-sitter-json))
   (native-inputs (list git))
   (synopsis "Extensions to org-mode")
   (description "Extensions to org-mode")
   (home-page "https://github.com/ApollonDeParnasse/org-x")
   (license gpl3)))

org-x
;;; guix.scm ends here
