(asdf:defsystem "static-site"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :description "A static startpage generator"
  :depends-on (:spinneret    ; HTML generation
               :cl-css       ; CSS generation
               :parenscript  ; JavaScript generation
               :iterate      ; Modern iteration
               :alexandria)  ; Utilities
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main" :depends-on ("package")))))
  :build-operation "program-op"
  :build-pathname "static-site"
  :entry-point "static-site:generate-site")
