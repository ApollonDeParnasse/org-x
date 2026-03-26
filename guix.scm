;;; guix.scm --- Guix package for generate

(define-module (org-x)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix build-system emacs)
  #:use-module (guix search-paths)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (packages generate))


(define-public org-x
  (package
    (name "org-x")
    (version "0.0.0")
    (source (local-file (getcwd) #:recursive? #t))
    (build-system emacs-build-system)
    (inputs (list emacs-dash emacs-s emacs-compat generate))
    (native-inputs (list git))
    (synopsis "Extensions to org-mode")
    (description "Extensions to org-mode")
    (home-page "https://github.com/ApollonDeParnasse/org-x")
    (license gpl3)))

org-x
;;; guix.scm ends here
