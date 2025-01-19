(use-modules (guix packages)
            (gnu packages lisp)
            (gnu packages lisp-xyz))

(package
  (name "static-site-dev")
  (version "0.1.0")
  (source #f)
  (build-system asdf-build-system/sbcl)
  (inputs
   (list sbcl
         cl-asdf
         sbcl-cl-css
         sbcl-parenscript
         sbcl-spinneret
         sbcl-iterate
         sbcl-alexandria))
  (native-inputs
   (list sbcl-rove))
  (synopsis "My static site generator")
  (description "Static site generator in Common Lisp")
  (home-page #f)
  (license #f))
